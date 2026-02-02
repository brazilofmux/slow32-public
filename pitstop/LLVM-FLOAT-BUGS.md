# LLVM Backend Float Bugs -- ALL RESOLVED

Found during regression testing of the new f32/f64 instruction set (2026-02-01).
All four bugs fixed and verified end-to-end (2026-02-01).

---

## Bug 1: Constant pool references missing %hi/%lo -- FIXED

LLC now emits `%hi(.LCPI0_0)` / `%lo(.LCPI0_0)` for constant pool references.
Verified by `feature-float-f32-convert` (`42+8.0f` test with constant pool literal).

## Bug 2: Float comparison against literal uses integer branch -- FIXED

LLC now emits `fle.s` / `flt.s` instead of `bgeu` for float comparisons.
Verified by `feature-float-f32-arith` (`abs(-7.5)` test).

## Bug 3: SETO (ordered) setcc not lowered -- FIXED

LLC now emits `feq.s rd, rs1, rs1` for ordered checks and `feq.s`/`feq.d` for equality.
Verified by `feature-float-f32-compare` (`a==c`, `a==b`, `e==e`) and
`feature-float-f64-basic` (`a==a`).

## Bug 4: f32 ↔ f64 conversion -- FIXED

Both directions work:
- f32→f64 (extending load → `fcvt.d.s`): verified by `feature-float-f64-basic`
- f64→f32 (truncating store → `fcvt.s.d`): verified by `feature-float-mixed` (`d2f+f` test)

---

## Regression test coverage

All tests exercise the previously-broken paths directly with no workarounds:

| Test | What it exercises |
|------|-------------------|
| `feature-float-f32-arith` | add, sub, mul, div, neg, abs (via `c < 0.0f`), zero ops |
| `feature-float-f32-compare` | eq, lt, le, gt, ge (including equality and self-equality) |
| `feature-float-f32-convert` | int↔float roundtrip, truncation, constant pool literal |
| `feature-float-f64-basic` | double arithmetic, compare (incl ==), int↔double, f32→f64 |
| `feature-float-mixed` | f32→f64 promotion, f64→f32 demotion, mixed int/float/double chains |
