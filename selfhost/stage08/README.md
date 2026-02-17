# Stage08: Pragmatic Archiver Parity Gate

This stage is a practical widening step before `cc.c` work: it hardens the
subset-C archiver (`s32-ar.c`) across the expected command surface.

Gate commands covered:
- `c` create
- `rc` replace/create
- `t/x` list/extract
- `d` delete
- `m` move-to-end
- `v/p` verbose/list + print-member
- `cs` bounded symbol-scan path

Run:

```bash
selfhost/stage08/run-regression.sh --emu ./tools/emulator/slow32-fast
```

First compiler-in-C spike:

```bash
selfhost/stage08/run-cc-spike.sh --emu ./tools/emulator/slow32-fast
```

Current `cc-min` pipeline shape (Stage08 multipass skeleton):
- Pass 1 (`pass1_parse_to_ir`): parse accepted subset-C into tiny IR state.
- Pass 2 (`pass2_validate_ir`): IR validation / codegen-prep checks.
- Pass 3 (`pass3_emit_from_ir`): emit target assembly from IR.

This preserves current behavior but provides explicit boundaries for
next-step features (for example variadic-function producer support).

Current `cc-min` accepted source shape:
- `int main(void) { return <const-expr>; }`
  where `<const-expr>` currently supports integer literals, `+ - * /`,
  unary `!`, relational/equality (`< <= > >= == !=`), and parentheses.
- `int main(void) { int x; x = <const-expr>; return x; }`
- `int main(void) { if (<const-expr>) return <const-expr>; return <const-expr>; }`
- `int main(void) { int x; x = <const-expr>; while (x) x = x - 1; return x; }`
- `int helper(void) { return <const-expr>; } int main(void) { return helper(); }`
- `int helper(int a) { return a [+/- <int>]; } int main(void) { return helper(<const-expr>); }`
- `int helper(int a) { int t; t = a [+/- <int>]; return t; } int main(void) { return helper(<const-expr>); }`
- `int helper(int a, int b) { return a + b; } int main(void) { return helper(<const-expr>, <const-expr>); }`
- `int helper(int a, int b) { if (a < b) return a; return b; } int main(void) { return helper(<const-expr>, <const-expr>); }`

`run-regression.sh` now runs both:
- archiver parity gate (`c/rc/t/x/d/m/v/p/cs`)
- `cc-min` end-to-end spike
