# Performance: where a COBOL step's time went, and where it goes now

The first pass, 2026-08-30, driven by majesty's `batch.sh` (nineteen
COBOL steps and thirteen Unix sorts over a 55,000-line ledger). The
whole batch took 1.83 s; the emulator ran ~14 G instructions across the
COBOL steps, six of them ~2.3 G each. After the pass: 0.64 s, 2.6 G
instructions, every report byte-identical.

## Method

Nothing here was guessed; each layer was measured before it was touched.

1. Time each step in place: wrap the emulator and `sort` in scripts
   that log `/usr/bin/time -p` (a Python timestamp costs 15 ms per call
   and swamped the first attempt).
2. Snapshot a step's working directory (its `tmp/` inputs, arguments)
   so it can be replayed alone.
3. Link with `s32-ld --print-map`, run the reference interpreter's trace
   over a window past start-up, histogram the PCs, attribute with
   `tools/utilities/s32-hotspots.py` (per function, then per
   instruction inside one function). `slow32-fast -p 1` samples the PC
   once a second with symbols and is enough to name the first suspect.

## What was found, in the order it fell

| layer | finding | fix | gl036 instructions |
|---|---|---|---|
| libc | `fgetc` = `fread(&c,1,1)` = a `memcpy` of one byte and `bytes / size` through `__udivsi3`, a 32-round shift-subtract loop, **per byte**: 88% of the program | `fgetc` takes the buffered byte itself; `fread`/`fwrite` skip the divide when `size == 1` | 2.83 G → 0.49 G |
| runtime | `__udivsi3`/`__divsi3`/`__umodsi3` were bit-serial although the ISA divides in hardware (signed only) | both operands under 2^31: one `div`; a big divisor: the quotient is `a >= b`; a big dividend: `(a>>1)/b` doubled and corrected once. Same in stage08's `builtins64.s` | (every unsigned divide, everywhere) |
| runtime | `__udivdi3` was 64 shift-subtract rounds; libc's `gmtime` (majesty's date bridge) divides a 64-bit `time_t` per call | a 32-bit divisor takes two hardware steps (Hacker's Delight `divlu`); both narrow, one `div` | |
| runtime | ...but only in `runtime/builtins.c`. The row above says "same in stage08's `builtins64.s`" of the *32-bit* routines; the 64-bit one was never mirrored, so on a host without LLVM -- where `builtins64.s32o` links ahead of `libs32.s32a` and wins the symbol -- every 64-bit divide still ran the 64 rounds (GitHub #30) | the same three fast paths transcribed into `builtins64.s` | `c9` 3509 → 2244 per `MOVE` |
| runtime | that left the divisor of 2^32 or more, which both copies still gave 64 rounds. A numeric `MOVE` that truncates divides by 10^13, and `mag_to_digits` then divides by 10^9, so `c9` does one of each: the 10^13 needs a **one-bit** quotient and was costing 1180 instructions to produce it | only `sr = msb(num) - msb(den) + 1` rounds can set a quotient bit; seed the loop with remainder `num >> sr` and counter `sr-1` and the rest are provably no-ops. The loop body is unchanged. Landed in `builtins64.s` first; the row below ports it to `runtime/builtins.c` | `c9` 2244 → 1149; `__udivmoddi3` 1279 → 92 per call |
| runtime | ...and `runtime/builtins.c`, the copy every LLVM-built program links, still ran the 64 rounds for a divisor of 2^32 or more (its own `divlu32` fast path stops at `den < 2^32`). LLVM constant-folds the ledger's 10^13 into multiplies, so the C copy only pays on an opaque divisor -- but it pays 64 rounds when it does | the same sr-seeded start, in C: `sr = nlz32(d_hi) - nlz32(n_hi) + 1`, remainder seeded with `n >> sr`, and the quotient held in 32 bits since `d >= 2^32` bounds it. `libs32.s32a` rebuilt with only `builtins.s32o` replaced | `run-builtins64-differential.sh` C side 125.1M → 80.5M instructions over the corpus, checksum `18191a74` unchanged; the assembly side stays 60.7M |
| libcob | numeric get/put: a 64-bit multiply or divide per digit; `%= 10^n` and `/ 10^n` by a runtime value through `__umoddi3`/`__divdi3` | nine digits at a time in a 32-bit word (the flush kept out of the character loop -- inside it the compiler if-converts it into a 64-bit multiply on every character); two digits a step through a pairs table; the power of ten a compile-time constant in every case | 0.49 G → 0.46 G |
| libcob | line-sequential READ through `fgetc`, ~35 instructions a byte | the runtime's own 8 K buffer and `memchr` | 0.46 G → 0.30 G |
| libcob | `cob_get_num` carried a 352-byte frame for the de-edit path's arrays under every numeric fetch; the packed store ran 17 instructions a digit through `nib / 2` | the de-edit path out of line; packed bytes straight from the digit pairs | |

The regression suite gained `feature-udiv-edge` and `feature-div64-edge`
(operands with the top bit set in every combination, INT_MIN / -1, the
signed remainder's sign); the COBOL harness (80) and the NIST suite
(303 programs, 7314 tests, 300 matching GnuCOBOL) are unchanged.

## Where it stands

Per batch: emulator 0.26 s over 21 launches, `sort` 0.23 s over 13, the
rest the pipeline's own serial shape. Inside a step the profile is now
the COBOL program's work -- `cob_get_num` ~36%, the record `memcpy`
~18% (its byte loop: COBOL fields are rarely word-aligned),
`cob_put_num_x` ~15%, `memchr` ~9% -- at ~5,000 instructions per
record. The next levers, none taken yet:

- **the compiler emitting the fetch itself** for a DISPLAY item whose
  descriptor is static, instead of `cob_get_num` reading it at run
  time (~80 instructions of call and dispatch around a ~10-per-digit
  loop);
- a `memcpy` that copies words at any alignment (the emulators allow
  unaligned access; the ISA text is silent), or one that aligns the
  destination and shifts;
- stage08's own libc, whose `fgetc` is a `read` system call per byte:
  it is what the self-hosted `cc.s32x` reads source through.
