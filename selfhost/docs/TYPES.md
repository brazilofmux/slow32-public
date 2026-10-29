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

**S4, source locations** (2026-10-28): `__FILE__`, `__LINE__` and
every diagnostic carry the presumed file and line (6.10.8.1).  The
splice's `#s12cc_pushdir` marker now names the included file, and its
handler saves the includer's name and line and starts the file at 1;
`#s12cc_popdir` restores them.  `#line` sets both, its operands from
macros if need be (6.10.4).  Diagnostics read `file:line: error: ...`
(`pp_where`), the header's own name and line.  Exactness was measured,
not assumed: every function in SQLite reports the line of its opening
brace (2,269 of 2,269), and every line number SQLite passes to
`sqlite3_log` names a line with its `_BKPT` macro (169 of 169; before,
2).  Three counting faults stood between: a macro call spanning lines
dropped its newlines (each two-line `assert` cost one -- now newlines
in a call are spaces, 6.10.3p10, and appended once to the expansion,
`pp_call_newlines`/`pp_keep_newlines`); an argument broken across a
line and used twice brought its newline twice; and the six
hand-written lexer peeks in parser.h did not restore the line on a
rewind -- they use `ps_lex_mark` now.  Probes `6.10.4-line`,
`6.10.4-line-macro`, `6.10.8.1-file-in-header`, `6.10.8.1-file-line`,
`6.10.3-line-after-call`.

**S4, macro replacement** (2026-10-28): the rest of W15 but `##`
forming a `##`.  Arguments are macro-expanded before substitution
unless `#` or `##` is applied to them (6.10.3.1): `pp_preexpand` copies
the argument past the end of the source buffer and expands it there in
place, with whatever is disabled around the call still disabled; a name
disabled there is *painted* -- a 0x01 byte in front of it, which the
lexer reads as an identifier never to be replaced (6.10.3.4p2).  So
`XSTR(VAL)` is `"42"`, `XSTR(__LINE__)` the line, and the expander's
second list -- argument regions exempt from disabling, which stood in
for pre-expansion -- is gone, and with it `m(m)` recursing until the
stack ran out.  Shifting text in place exposed an off-by-one the old
in-place path (used only under a parser peek) had too: an expansion
whose last token was the replaced call did not move with it, and stayed
disabled past its end.  Stringizing became `pp_stringize`: escapes
only inside literals, a stray `\` copied bare; comments and newlines
in a call are spaces before anything sees them (a `//` comment, its
newline gone, had swallowed the rest of the expansion); `#__VA_ARGS__`
spells the variable arguments.  `#if` tracks uintmax_t through the
usual conversions (`-1 < 0u` is false) and has `?:`, which it never
had -- the probe passed because the tokens after the condition were
skipped.  One finding off the path: `offsetof` was a lexer keyword, so
SQLite's own `#define offsetof` held inside a pre-expanded argument and
nowhere else.  stddef.h now defines it as `__builtin_offsetof`, and a
program's macro of that name is expanded.  With that, SQLite's assembly
is byte-identical: the new expander is token-neutral on 265,876 lines.
The difference had shown a real defect, next: `sizeof` and `offsetof`
are `int`, not `size_t`.  Probes `6.10.1-if-unsigned`,
`6.10.3-variadic-stringize`, `6.10.3.2-stray-backslash`,
`6.10.3.2-stringize-comment`, `6.10.3.2-xstr`, `6.10.3.3-placemarker`,
`6.10.3.4-painted-arg`, `6.10.3.5-example4-str-glue`,
`6.10.3.5-example7-variadic` (WRONG to PASS), `6.10.3-empty-args`,
`6.10.3.5-example3-rescan` (REJECTS to PASS).

**`sizeof` and `offsetof` are `size_t`** (2026-10-28): they were
`int`, so `(n + 1) * sizeof(double)` sign-extended into a `long long`,
`-sizeof(struct s)` was negative, and `n < sizeof(int)` compared
signed (6.5.3.4p5, 7.19p2).  `nd_size` gives them `unsigned long`,
as stddef.h spells `size_t`.  It made `sizeof(a) / sizeof(a[0])` a
call to `__udivsi3`: unsigned division by a constant was never folded
(`116u / 4u` was a call too).  `hl_udivmod32` now folds two constants,
and a power-of-two divisor is a shift or a mask, as clang does --
SQLite has 19 fewer such calls than before.  Probe
`6.5.3.4-sizeof-size-t`.

**S5, the preprocessor's constraints** (2026-10-28): the 30 ACCEPTS in
6.10 are now errors (`pp_error`, at the presumed file and line).  The
if-section's shape -- `#else` once and last, `#elif` not after it,
`#endif` with an `#if`, every section closed in the file it opened in,
in skipped groups too; `#if` must be one whole constant expression (an
empty one, a dangling operator, `=`, a floating constant), `defined`
needs a name; an unknown directive name in a live group; `#include`
and `#line` operands (and `#include MACRO` now works, 6.10.2p4);
`#define`'s rules -- unique parameters, white space after an
object-like name, `__VA_ARGS__` only in a variadic macro, `#` before a
parameter, `##` at neither end, `defined` and the predefined names
neither defined nor undefined; a redefinition must match (6.10.3p2: kind,
parameters, and the list with white-space runs as one space --
`pp_dsig`); too many arguments; and a `#` begins a directive only as a
line's first token (`lex_bol`), so `EMPTY # define Z 1` is not one.
Four findings on the way.  `#if` never expanded a function-like macro:
SQLite's `__has_extension(c_atomic)`, defined as 0, came out 0 because
the call was dropped with the rest of the line (`pp_ev_call`).  `#if`
character constants skipped escapes the same way.  A parser peek that
lexed past a directive ran it twice -- SQLite's `= {` then `#ifdef
SQLITE_ASCII` pushed a level that stayed to the end of the file; a
lexer mark now saves the conditional state (`pp_cond_save`).  And the
compiler read SQLite within a megabyte of its heap: s12cc.c kept the
12MB input buffer after copying it into `lex_src`, and a `strdup` that
came back NULL showed up as an unterminated `#if` 54,000 lines on.
SQLite's assembly is byte-identical.

**S5, expression constraints** (2026-10-28): every node now records
where it was read (`Node.line`, `Node.file`, an index into the interned
`pp_files`), so sema's diagnostics carry `file:line`.  `sema_constraints`
runs on each expression node once its operands are typed and before
sema's conversions wrap them in casts: integer-only operators (`%`, `&`,
`|`, `^`, shifts, `~`), arithmetic `*` and `/`, the forms `+` and `-`
allow (no pointer + pointer), scalar operands for `!`, `&&`, `||` and
`?:`'s condition, `*` only on a pointer, assignment, compound assignment
and `++`/`--` only to a modifiable lvalue (the program's own `=` is
marked `op = TK_ASSIGN`, an initializer's is not), casts only to void or
a scalar from a scalar and never between pointer and floating, a void
value used as one, a call's argument count against its prototype.
The conversions 6.5.16.1 constrains -- assignment, initialization,
argument, return, and the pointer comparisons -- are written
(`sema_assign_check`) but held back: `S12CC_SURVEY=1` reports each by
class, and on SQLite they are mostly our own C types, not SQLite's
(`const char *z2 = z;` reported as dropping a qualifier, a cast to a
function-pointer typedef as an integer).  They are enabled class by
class as the types become exact.  Three such gaps fixed on the way,
each a false error first: a local function pointer's C type took the
last parameter's type for its return (`int (*fp)(void)` returned
void), a local pointer to function pointers fell back to int, and a
compound literal was not an lvalue (it is a comma, its stores and then
its variable; the comma is marked).  The argument count found two more,
in the corpus: a prototype with an unnamed `void *` parameter --
`void sqlite3_free(void*);` -- was recorded as taking none, because
`parse_type` takes the star and the `(void)` test saw the `)`; and a
pointer to a function returning a function pointer (`void
(*(*xDlSym)(void *, const char *))(void)`, as member or parameter) was
typed one function level short, its call a void value.

The survey, run over SQLite, then the whole corpus through the gates
(`S12CC_SURVEY_LOG=path` appends the reports to a file), found the C
types' remaining gaps before it found SQLite's violations: a `const`
read with the storage class (`static const char *z`) never reached the
C type -- every such local's and global's pointee was unqualified
(`ps_storage_next`); `ps_parse_fp_params` left the last parameter's
type in `ps_type_cty`, so an unnamed function-pointer parameter after
the first (`sqlite3_result_text(..., void(*)(void*))`) was an int;
`va_arg(ap, T)` had no C type; `?:` on two pointers fell back to the
machine type; and a pointer to a function pointer at any depth (`void
(**aExt)(void)`) did not agree with its int machine spelling and fell
back to int.  SQLite now has no report left.

The corpus then.  More type gaps: a global array of a function-pointer
typedef (`static H tab[4]`, initialized or not) and an uninitialized
`static int (*m[2])(long long)` had int elements; a prototype recorded
a parameter's machine type where its variable had the C type (a row
pointer `char (*rows)[8]`).  An initializer's own stores into members
and elements are made by offset, their targets typed from the machine
type, so for now only an initializer of a whole variable is checked
(a gap: member initializers).  One finding was a miscompile: a
compound literal of character array type initialized by a string,
`(char[]){"abc"}`, took the string for its one element and stored its
address into a char -- the array was empty (`ps_cl_string`; probe
`6.5.2.5-compound-literal-string`).  And real violations, in our own
code, fixed where they were: the compiler declared `memcpy` with
`char *` parameters and called `malloc`, `calloc` and `strdup` with no
declaration at all (an implicit int, assigned to pointers) -- the
dialect block in c_lexer.rl now has the library's types; the selfhost
`<unistd.h>` had `read`, `write` and `unlink` without `void *` and
`const`.  (And one more gap: a block-scope `static int
(*slot[2])(long long)` had int elements, `ps_add_static_fnptr_alias`.)

**Ruling, 2026-10-28: 6.5.16.1's conversions are graded as clang grades
them by default.**  What the corpus left was DOOM passing callbacks
through `void *` (`M_StartMessage`'s routine, the save code's action
pointers).  Clang's own grading, measured: an integer converted to or
from a pointer, incompatible pointer types (function pointers among
them), a difference of incompatible pointers and a call of something
not a function are errors; a dropped qualifier, a pointer-sign
mismatch, a pointer compared with an integer or with a pointer of a
distinct type, and `?:` arms of mismatched types are warnings; a
function pointer converted to or from `void *` is accepted -- an
extension, which POSIX's `dlsym` depends on and DOOM uses.
`sema_convert_diag` applies it; `-pedantic-errors`, the probes' oracle,
would make all of these errors, so the probes for the warning classes
stay ACCEPTS by this ruling, not by a gap.

**S5, statements and lexing** (2026-10-28).  The same principle,
clang's default grading, for what remained: measured without
`-pedantic`, nearly every remaining ACCEPTS is a hard error in clang,
and four are GNU extensions clang takes unless `-pedantic` -- a label
before a declaration, an enumeration declared before its definition,
an enumerator outside int, a zero-length array -- which stage08 takes
too, as extensions; a string longer than its char array is a warning.
Statements (sema): `if`, `while`, `do` and `for` need a scalar
controlling expression, `switch` an integer one; `case` and `default`
only inside a switch; within one switch no two case values equal after
conversion to the promoted controlling type, and one default
(`sema_switch_check`); a case label must be an integer constant
expression (`ps_case_value`: `case 1.5` was 1); a label once per
function (`ps_define_label`); `return` with a value from a void
function, without one from a non-void function, and its value as if
assigned (`sema_return_check`).  Lexing: an octal constant with an 8
or 9, a constant too large for any integer type, an empty character
constant, a hexadecimal pp-number running on through `e+` (`0x1e+1`),
and `_Complex`/`_Imaginary`, keywords stage08 does not implement
(`lex_err`, reported by `next()` where the token is).

**S5, declarations: the local rules** (2026-10-28).  The storage-class
words of a declaration all pass `ps_storage_next` now: at most one
storage class; neither `auto` nor `register` at file scope, a function
included; `register` recorded on the local, and `&` of one refused;
`inline` on an object, or on `main`, refused (`ps_top_inline`).  A
leading `restrict` must qualify a pointer; an object may not have type
void; a function may not return an array or a function, and `void` is
a parameter list only alone; an enumerator's value, like a case
label's, is an integer constant expression (`ps_require_integer_const`:
`E = 2.5` was 2); `sizeof` of a bit-field or an incomplete structure;
`_Atomic` of an array or function type; and `_Static_assert` evaluates
its expression -- before, only a bare literal was looked at, anything
else passed.

**S5, redeclarations** (2026-10-29).  A tag used as the other kind
(`struct t` then `union t *p`) where it refers to the earlier one
(`ps_find_tag`); a tag defined twice in one scope
(`ps_tag_defines_here`); two members of one name, anonymous members'
included (`ps_check_dup_members`); a typedef redefined in its scope as
a different type (`ps_typedef_check`; the compiler's own -- `size_t`,
`bool` -- are left to the headers); a function redeclared with an
incompatible type (`ps_func_redecl_check`) or given two bodies
(`ps_func_defined`); a global redeclared with an incompatible type --
where either declaration is an array, by its innermost elements: an
array declared before its size is known is held in pointer form, and a
2-D one flat or by rows, so `extern int a[]; extern int a[3];` and
DOOM's `scalelight[LIGHTLEVELS][MAXLIGHTSCALE]` (r_state.h, r_main.c)
are one array each (`ps_global_types_agree`; array sizes are not
compared) -- and
`static` after a declaration with external linkage
(`ps_global_redecl_check`); a local declared twice in one scope -- a
block, or a `for` statement's first clause, which is a scope of its
own (`ps_scope_begin`/`ps_scope_end`, `ps_lscope`).  One finding:
`#pragma once` was skipped with the other pragmas, and a header that
relied on it, included twice, defined its function twice -- accepted
until now; it is implemented (`pp_once`).

**S6, C11's own** (2026-10-29).  `_Alignof` (a type name; GNU's
`__alignof__` an expression too), `size_t` (`ct_alignof`); `_Generic`,
the association whose type is compatible with the controlling
expression's after lvalue conversion -- typed by sema, not evaluated --
with no two associations compatible (`ps_parse_generic`); `__func__`,
a string of the function's name; `_Noreturn`, a promise with nothing
to emit; `_Thread_local`, static storage in a one-threaded program,
needing `static` or `extern` at block scope; `<stdalign.h>`,
`<stdnoreturn.h>`.  `_Generic` sees what sizes never did: on SLOW-32
`size_t` and `ptrdiff_t` are `unsigned int` and `int`, as clang's
target has them (stddef.h said `unsigned long` and `long`, the right
size and the wrong type; 64-bit hosts keep those); `1L` is a `long`
and `1UL` an `unsigned long`, the size of an int and other types
(`lex_val_l`; the parser names the type, sema keeps it); and a
constant past `long long`'s range is `unsigned long long`
(`0xFFFFFFFFFFFFFFFF` was a signed -1).  `max_align_t` added.

`_Alignas` (6.7.5), a type name or a constant, among the storage-class
words or the qualifiers (`ps_read_alignas`), the strictest of a
declaration's kept: on a global, the `.align` before its label
(`hcg_overalign`; the assembler keeps each section's strongest and the
linker places the section there); on a structure member, its offset
and the structure's alignment and size; never weaker than the type's
own, never on a bit-field or in a typedef.  A local's stronger than a
word is refused: the frame is word-aligned (clang's SLOW-32 target has
4 as its biggest alignment too), and it would take realigning the
stack at entry -- a gap, two probes.

**C11 claimed.**  `__STDC_VERSION__` is 201112L.  The optional features
not implemented are declared, as 6.10.8.3 has an implementation do:
`__STDC_NO_COMPLEX__`, `__STDC_NO_VLA__`, `__STDC_NO_THREADS__`,
`__STDC_NO_ATOMICS__`.  A probe of one of them carries
`// optional: __STDC_NO_X__`, and when stage08 defines that macro and
refuses the probe, or answers otherwise than clang (which has the
feature), the verdict is OPTIONAL: a choice, not a gap (ten probes).

**The REJECTS: address constants** (2026-10-29).  A static initializer
that is an address constant (6.6p9) is evaluated on its type, not read
by its tokens: the whole expression is parsed, typed by sema, and
walked to a base -- a global, a static local, a function, a string
literal -- and a byte offset: members add theirs, `*` and `[]` follow
the address, `+` and `-` with an integer constant scale by the pointee's
C type, casts pass through (`ps_addr_value`, `ps_addr_lvalue`,
`ps_init_addr_const`).  The token reader took a name and stopped at the
`+` or the `.`: `garr + 4`, `&gs.b[2]`, `&garr[5] - 2`, `"hello" + 1`
and `(char *)&gs + sizeof(int)` were refused.  It still reads what the
evaluator does not claim, and what both read gives one relocation, so
SQLite's assembly is byte-identical.  A string's relocation carries its
addend now.  A compound literal at file scope has static storage
(6.5.2.5p5): an anonymous TU-local global initialized from its braces
(`ps_file_compound_literal`), where it had been built as a hidden local
there are none of.

**Implicit function declarations refused** (2026-10-29).  A call to a
function not declared is an error, as C99 made it and as clang's
default has it (6.5.1p2; probe `6.5.1-implicit-function`) -- graded
with the conversion classes, `implicit-fn`, so `S12CC_SURVEY_LOG`
finds them.  The case for it came from the compiler itself:
`sema_cvalof`, called in parser.h above its declaration in sema.h,
was taken for an implicit int and its long long result cut to 32
bits, which the host-compiled cross drivers caught.  The corpus had
eleven such calls.  Two were not violations but a parser gap: a
function declared at block scope (DOOM's `void WI_unloadData(void);`
inside the caller) was skipped, its call then an implicit
declaration; it is registered now (`ps_local_prototype`).  The rest
were real, in our own code: the compiler used `write`, `strchr`,
`strcpy`, `strncpy` and `rename` undeclared (the dialect block in
c_lexer.rl declares them), and the libc's `posix_proc.c` called
`abort` above its definition.

**REJECTS: declarators** (2026-10-29).  A `for` statement's first
clause is a declaration and is read as one, by parse_stmt
(`ps_for_decl`): several declarators, an initialized array, a struct
type defined there, `register` -- it took one name, its stars and an
`= expr`.  A single `name = value` stays the loop's init expression, so
the loops the unroller knows look as they did.  The arithmetic type
specifiers in any order (`ps_arith_specifiers`: `int signed`, `int long
long`, `short unsigned int`; and two of signed and unsigned, or `long
short`, refused).  `register` on a parameter; qualifiers and `static`
inside an array parameter's brackets (`int a[restrict static 1]`);
`_Static_assert` among a structure's members; one typedef declaring
several names (`typedef int A, *AP;`).

**REJECTS: tentative definitions and the preprocessor** (2026-10-29).
`int arr[];` with no initializer is a tentative definition, one element
unless a later declaration completes it (6.9.2p2) -- not with internal
linkage (6.9.2p3), and a negative size is refused where the size is
read (`ps_array_size`; -1 had been both "negative" and "no size");
`extern struct pt pv;` may come before `pt` is complete.  `#include`'s
operand is macro-replaced whole, as text (`pp_preexpand`), so
`#include STR(inc/v1.h)` works; a function-like macro's `(` may come on
a later line, its newlines counted; `##` in an object-like macro is
pasted when it is defined; `__DATE__` and `__TIME__` exist, from the
clock or `SOURCE_DATE_EPOCH`; and the `_Pragma` operator, from a macro
too, `_Pragma("once")` as `#pragma once`.  One expander defect: in
place (under a parser peek) a function-like expansion went in at the
`(` and left the name before it, read again after the rewind,
disabled by then -- `k CAT(+, =) 4` reached the parser as `k CAT += 4`.
The name is replaced with the call now.  `S12CC_CONSTRAINT_WARN=1` turns
the errors into warnings, to survey a corpus.

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
