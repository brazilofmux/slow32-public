# C11 coverage: stage08 cc, 2026-10-28

The first full survey: 516 probes over chapter 6, every subclause given
at least one row on its page.  Totals from one run of `./probe.sh`:

| verdict | probes | |
|---|---|---|
| PASS | 159 | valid C11, same result as clang |
| DIAG | 79 | invalid, refused (about 20 only because the feature is missing; the pages mark them) |
| WRONG | 52 | valid, compiles, wrong result |
| REJECTS | 89 | valid, refused |
| ACCEPTS | 137 | invalid, taken without a word |

| page | PASS | DIAG | WRONG | REJECTS | ACCEPTS |
|---|---|---|---|---|---|
| [6.2-6.4](c11-6.2-6.4.md) concepts, conversions, lexical | 41 | 9 | 11 | 23 | 15 |
| [6.5-6.6](c11-6.5-6.6.md) expressions, constant expressions | 31 | 15 | 12 | 17 | 36 |
| [6.7](c11-6.7.md) declarations | 26 | 27 | 7 | 24 | 32 |
| [6.8-6.9](c11-6.8-6.9.md) statements, external definitions | 26 | 23 | 5 | 13 | 25 |
| [6.10](c11-6.10.md) preprocessing | 35 | 5 | 16 | 12 | 29 |

(Page counts are each agent's own run; the totals above are the whole
set at once, with one probe added since, `6.5.5-double-times-pow2-int`.)

**Progress.**  2026-10-28, selfhost ISSUES-100: W1, W3, W4 (except the
2-D `2[m][1]`), W5, W7, W8, W9, W10 and W14's octal escapes fixed, and
`&function` accepted; `probe.sh --check` against `verdicts.txt` now
gates every change.  Then the type work (docs/TYPES.md): S3 fixed W2,
`_Bool`; comparison and pointer-difference types; W12, `sizeof` of
computed values and bit-field promotion; W13, bit-field layout now
clang's (197 PASS, 25 WRONG, 83 REJECTS, 81 DIAG, 135 ACCEPTS).  The tables below are the survey as
taken.

The 52 WRONG probes come from about fifteen defects, listed below by
root cause, most dangerous first: a program that compiles and silently
computes the wrong thing is worse than one that is refused.

## 1. Wrong code

| | defect | probes | reach |
|---|---|---|---|
| W1 | **A double or float times an integer constant that is a power of two is miscompiled**: `d * 2` returns `d`, `d * 4` returns `d / 2` -- an `fdiv.d` where the multiply was; a strength reduction meant for integers applied to a floating operand.  `d * 2.0`, `d * 3`, `d * k` are right | `6.3.1.8-double-times-int`, `6.5.5-double-times-pow2-int` | any floating code that scales by 2, 4, 8 ... |
| W2 | **`_Bool` is a typedef for `unsigned char`** (`add_typedef("_Bool", ...)` in parser.h): conversion to `_Bool` truncates instead of testing against zero (`_Bool b = 256` is 0, `= 0.5` is 0, `= &x` is an address byte), `b++` on true gives 2, `(_Bool)2 + 1` is 3, a `_Bool` switch misses `case 1` | `6.3.1.2-bool`, `6.5.16.1-assign-bool`, `6.5.2.4-postfix-bool`, `6.5.3.1-prefix-bool`, `6.5.4-cast-bool`, `6.6-const-bool`, `6.7.2-bool-object`, `6.8.4.2-switch-bool`, `6.8.6.4-return-bool` | every `bool` from `<stdbool.h>` given anything but 0 or 1 |
| W3 | **A label reached only by an unconditional `goto` into its block empties the whole function**: `goto in; { in: x += 7; }` compiles with status 0 to `main:` and two empty labels (the probe sees the link fail).  An undefined label does the same | `6.2.1-label-scope`, `6.8.6.1-goto-into-block`, `6.8.6.1-goto-into-loop`, `6.8.6.1-goto-undefined-label`, `6.8.6.1-goto-other-function` | silent loss of a function |
| W4 | **integer + pointer is not scaled**: `3 + ia` adds 3 bytes; `ia + 3` is right, so `2[a]` reads garbage | `6.5.6-int-plus-ptr`, `6.5.2.1-subscript-commuted` | rare spelling, total corruption |
| W5 | **postfix `++`/`--` on a `double` leaves it unchanged** (`float` works) | `6.5.2.4-postfix-double` | loops with a double counter |
| W6 | **static initializers fold wrongly**: double arithmetic (`2 + 0.25` stores 2.0, `1.0/4 + 2.0` stores 0.17); `int * long long` folded in 32 bits (`0x7FFFFFFF * 2LL` stores -2) | `6.6-arith-const-double`, `6.6-arith-const-llong` | tables of constants |
| W7 | **`switch` on `long long` compares the low word only**; a switch whose body is a single unbraced statement is miscompiled | `6.8.4.2-switch-longlong`, `6.8.4.2-switch-unbraced-body` | |
| W8 | **falling off the end of `main` does not return 0** (5.1.2.2.3, probed under 6.9.1) | `6.9.1-main-fall-off` | exit status of many small programs |
| W9 | **`*f` / `**f` on a function designator load through it**; calling the result faults | `6.3.2.1-func-designator` | |
| W10 | **block-scope `extern int x;` binds to a visible local `x`** instead of the file-scope object | `6.2.2-block-extern-shadow` | |
| W11 | **declarations read wrongly**: `sizeof(int (*)[3])` is 12 (pointer to array read as array of pointers); an inner struct tag does not shadow the outer; block-scope enumerators leak out of their block; `char a[] = {"hi"}` is one empty byte | `6.7.7-type-names`, `6.7.2.3-block-tag-shadow`, `6.7.2.2-enum-scope`, `6.7.9-braced-string` | |
| W12 | **promotions and result types**: an unsigned bit-field narrower than `int` stays unsigned; `sizeof(+c)`, `sizeof(d + d)`, `sizeof(1 << ll)` are wrong (values right, types wrong) | `6.3.1.1-*`, `6.3.1.8-sizeof-arith`, `6.5.3.3-unary-promote-type`, `6.5.7-shift-result-type`, `6.7.2.1-bitfield-promote` | |
| W13 | **bit-field layout differs from clang's** (`{unsigned x:3; _Bool f:1;}` is 8 bytes, clang 4): implementation-defined, but it breaks the shared ABI the two compilers otherwise keep | `6.7.2.1-bitfield-layout` | interop |
| W14 | **lexing**: an octal escape beginning `\0` stops after the 0 (`'\001'` is 0); `\u`/`\U` in literals are not translated; a multi-character constant differs from clang's value | `6.4.4.4-octal-zero`, `6.4.5-escapes`, `6.4.3-ucn-string`, `6.4.4.4-multichar` | `"\001"`-style data |
| W15 | **preprocessor**: `__LINE__` counts the whole translation unit after an `#include` (and so do stage08's own error lines), `__FILE__` in a header names the includer, `#line` is ignored; arguments are not macro-expanded before an inner `#`/`##` (`XSTR(VAL)` gives `"VAL"`); `#__VA_ARGS__` is `""`; stringizing keeps comments and escapes a stray `\`; `##` forming a punctuator and placemarkers; `m(m)` re-expands; `#if` is signed-only (`-1 < 0u` true); `__STDC_VERSION__`, `__STDC_HOSTED__` (and `__DATE__`, `__TIME__`) undefined | `6.10.*` (16) | assert messages, the stringize idiom, version tests |

## 2. Valid C refused

Clustered; the pages have the probes.

- **The C11 keywords**: `_Alignas`, `_Alignof`, `_Generic`, `_Noreturn`,
  `_Thread_local` are not recognised (the error is "expected token 56 got
  27"); `_Complex` is refused without `__STDC_NO_COMPLEX__` defined.
- **Address constants and functions**: `&function` ("not an lvalue",
  in a block and in a static initializer); static initializers accept
  only `&a[i]`, an array or a function name -- not `arr + 4`,
  `"lit" + 1`, `&s.m`, `(char *)&s + 4`; a few constant forms are
  parse errors (`0.25 + 2`) or false errors (`3 / 2.0f` "division by
  zero", 64-bit multiply "not supported").
- **Declarators**: pointer-to-array objects and functions returning one;
  function-typed parameters; several declarators in one declaration
  (`int x, (*fp)(void), f(void);`, `typedef int A, *AP;`); specifiers out
  of the usual order (`int signed`); `[static N]` / `[const]` in
  parameters; `_Static_assert` in a struct; `register` on a parameter.
- **Initializers**: any brace initializer that sets a bit-field, including
  `= {0}`; an initialized array typedef; file-scope compound literals.
- **Statements and definitions**: `for` declarations of more than one
  plain declarator; K&R function definitions; `int a[];` tentative
  definitions; `extern struct S v;` before `struct S` is complete;
  `(t ? a : b).m`; `__func__`.
- **VLAs**: none, which C11 permits -- but then `__STDC_NO_VLA__` must be
  defined, and it is not.
- **Lexical**: `L`/`u`/`U`/`u8` prefixes, hex floating constants,
  digraphs, UCNs in identifiers.
- **Preprocessor**: `_Pragma`; computed `#include MACRO` (skipped
  silently); an empty macro argument; `##` in an object-like macro; a
  function-like macro whose `(` is on the next line.

## 3. Invalid C accepted

137 probes, nearly all from two absences rather than many slips:

- **There is no type checker for 6.5's constraints.**  Accepted: calls
  with the wrong number or types of arguments against a prototype,
  calling a non-function, int/pointer mixing without a cast, dropping
  `const`, incompatible pointer types, pointer + pointer, wrong operand
  types for most operators, `sizeof` of an incomplete type, `&` of a
  `register` object, assigning to an array or to a struct of another
  type, using a `void` value, `void v;`.
- **`const` is not enforced at all** (assignment, `++`, storing through
  `const T *`), which is ISSUES-99's front-end half: the parser now
  records const for placement, not for checking.
- **`_Static_assert` fails only on a literal 0**: any other false or
  non-constant condition passes.
- **No redeclaration or redefinition checks**: objects, functions,
  typedefs, tags, struct/union tag reuse, duplicate members, a function
  defined twice, `int x; static int x;`.
- **Statements**: `case`/`default` outside a switch, duplicate cases, two
  defaults, `case 1.5`, switch on a double or pointer, duplicate labels,
  the `return` constraints.
- **The preprocessor diagnoses almost nothing** beyond a missing include
  and `#error`: bad `#if`, unbalanced conditionals, macro redefinition,
  argument-count errors (extras dropped), misplaced `##`, unknown
  directives.
- **Lexical**: `09`, an integer constant too large for every type, `''`,
  `0x1e+1` read as `0x1e + 1`.

## 4. Headers (clause 7, noted only)

stage08's `include/` lacks `<float.h>`, `<stdalign.h>`, `<stdnoreturn.h>`;
its `<stddef.h>` lacks `wchar_t`; neither libc's `<assert.h>` defines
`static_assert`.

## Suggested order

1. **W1-W10**: wrong code first, smallest fixes with the widest reach --
   W1 (float scaling) and W2 (`_Bool`) above all; W3 loses functions.
   Each probe becomes a stage08 test as it is fixed, and cfuzz should
   learn the pattern that slipped past it (W1: an integer literal
   multiplying a double).
2. **W15 and the preprocessor REJECTS**: `__LINE__`, argument
   pre-expansion and `__STDC_VERSION__` break real headers and
   assertions; with it, define `__STDC_NO_VLA__`, `__STDC_NO_COMPLEX__`,
   `__STDC_NO_THREADS__` -- then `__STDC_VERSION__ 201112L` can be honest.
3. **W11-W14, then the common REJECTS**: `&function`, address constants,
   declarators, bit-field initializers, the C11 keywords.
4. **Diagnostics as their own project**: a type checker for 6.5, const,
   redeclarations, `_Static_assert` evaluated properly, and a preprocessor
   that complains.  That is most of the 137, and the one piece with no
   wrong code behind it: worth doing, but after the compiler stops
   miscompiling valid programs.
