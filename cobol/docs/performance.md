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

## 2026-09-30: integer arithmetic in registers, and cheaper activations

**The workload.** majesty's `batch.sh` is small now: 0.28 s for the whole
run, and 52 ms for its heaviest program. So the measure is jerm, the
-std=2002 date-functions build (400,001 lines, from
`tests/majesty-functions.sh`). It spent 26.4 G guest instructions:
- about 70% in the decimal stack, for integer COMP-5 arithmetic
  (floor-divmod's `DIVIDE ... REMAINDER`, `COMPUTE 400 * n400 + ...`);
- about 12% in calling user functions (a `malloc`, `free` and word
  copies per call);
- about 4% in its own compiled code.

| where | what it cost | what changed | jerm |
|---|---|---|---|
| compiler | MULTIPLY, DIVIDE and COMPUTE always went through the decimal stack (push, `cob_ndiv`, store), even on binary integers; ADD and SUBTRACT already had a hot path | integers computed in a word (`hx_*` in s32-cobc.c), in one of three modes: **exact** when every intermediate provably fits; **wrap** when every receiver is signed COMP-5 or native, whose stored low bytes the stack's store leaves too; **checked** otherwise, each `+ - *` and negation testing for overflow (`mulh` for a product) and branching to the stack's code for the whole statement. Division only at the top (COBOL's `7 / 2 * 2` is 7). A zero divisor leaves the receivers; -1 is negation. SIZE ERROR, the EC checks and ROUNDED on a quotient keep the stack | 26.4 G → 6.4 G |
| libcob | `cob_act_enter` did a `malloc` and copied the saved words on every call, and `cob_act_leave` copied them back and freed the block | the outermost activation, nearly every call, keeps one cached block and saves nothing; on re-entry its cells already point into the block, so only the initial images go back | 6.4 G → 5.7 G, with the next row |
| compiler | each function-result temporary (`.Llft`, per-activation in a recursive unit) had its initial image copied on every call | the descriptor gives it no image: its call writes it before anything reads it | (in the row above) |

- **The result:** jerm runs in 0.56 s on slow32-dbt, down from 2.37 s,
  with its output byte-identical.
- **`-fno-hot-arith`** turns the register path off, which gives a
  differential's other side. With it (and `-fbinary-byteorder=native`),
  the Open Systems suite compiles byte-identical to the baseline.
- **Tests:** `tests/free/hotmuldiv` and `hotarith2` cover signs,
  remainders, zero and -1 divisors, truncation, wrapping and overflow
  fallbacks. GnuCOBOL agrees with both.

**What is left in jerm:** moving COMP-5 values into DISPLAY and edited
output fields (`cob_k_put_num`, `cob_move`, `cob_edit_apply`), about a
third of it. That is the next lever: inline decimal and conversion code
in the compiler, as in the "Where it stands" list above.

## 2026-09-30, continued: decimal arithmetic in registers

**The change.** What the integer path cannot take now also leaves the
decimal stack:
- decimals;
- packed and DISPLAY items of up to 18 digits;
- ROUNDED;
- a division at the top of an expression.

`dx_*` in s32-cobc.c computes these as 64-bit scaled integers in
register pairs, with every scale known when the program is compiled.

**Staying the stack's answer.** Every intermediate is bounded below
9 * 10^18, and a product's scale may be at most 18. So none of the
stack's digit shedding ever happens, and alignment, sums and products
are exact, as the stack's are.

**Operands and results.** Operands are fetched by `cob_get_num` and the
result is stored by `cob_put_num_x`. These are the stack's own fetch and
store: they round, truncate and edit, and the DBT runs them natively.
What goes away is the stack between them: its pushes, alignment and
dispatch.

**Division** calls `cob_xdiv`, the stack's long division (now shared as
`ndiv_core`), and reads the quotient's scale at run time. A zero
divisor leaves the receivers unchanged.

**ADD and SUBTRACT with several receivers** sum the operands once, then
add that sum to each receiver, as `cob_top_addto` does. MULTIPLY and
DIVIDE re-read their operand for each receiver, as the stack does.

**The stack still takes:**
- SIZE ERROR and the EC checks;
- a remainder of decimals;
- `**`;
- 31-digit, float and P-scaled items;
- an expression too deep for the frame's slots.

**The integer path**, alongside: a PIC 9(9) COMP item (unsigned, four
bytes) now joins it. Only one that keeps its capacity (COMP-5, the
native types) can use the top bit.

**Two scan hazards found on the way, both fixed:**
- **A user function's result met while scanning ahead.** The scan
  makes no call, so a register tree may not hold that result
  (`opnd_scanned`).
- **CCVS NC252A.** Its expressions nest deeper than the frame's slots,
  so each path now counts the slots it needs and declines past them.

| kernel | guest instructions (before -> after) | slow32-dbt |
|---|---|---|
| karith (COMP-3/DISPLAY COMPUTE ROUNDED, ADD, DIVIDE, MULTIPLY) | 24.9 G -> 11.8 G (what is left is mostly the fetch and store, which the DBT runs natively: hence the time falls further than the count) | 1.57 s -> 0.49 s |

`tests/free/hotdec` must equal the stack's output, and does: `-fno-hot-arith`
gives the stack's answers, 98 stack calls in it against 14. GnuCOBOL
agrees with every line.

## 2026-09-30, continued: intrinsics, MOVE, INSPECT, compares

**Choosing what to fix: the guest-only view.** Under slow32-dbt the
following run natively:
- memcpy, memset, memmove, strlen, memcmp (intrinsics);
- the 64-bit divides, `cob_get_num`, `cob_put_num_x`, `cob_get_edited`
  and `cob_put_edited`, with the kernels beneath them (hooks).

So the interpreter's profile overstates them. The targets below were
picked from the profile with those names filtered out.

**Intrinsics in registers.** FUNCTION MOD and REM with a literal divisor
of magnitude 2 or more (so no zero and no INT_MIN / -1), INTEGER,
INTEGER-PART and ABS are now nodes of both register trees. Their
arguments may be items, literals or expressions. In the integer path
they compile to `rem` with a floor adjustment; in the decimal path to
`__moddi3` and `__divdi3` (both hooked).

**Numeric MOVE.** `cob_move` does `cob_put_num(cob_get_num)` for numeric
to numeric, so `dx_move` emits that fetch and store directly. The
source may be an item, a literal or one of the intrinsics above.

**INSPECT.** When every phrase is CHARACTERS or ALL of one byte, over
the whole item, a 256-entry table (the first phrase listed wins) does it
in one sweep instead of trying every phrase at every position.

**Compares.** A reference modification of constant length joins the
in-line one-byte compare, and the memcmp compare against a literal of
its length.

| kernel | before | after (slow32-dbt) |
|---|---|---|
| kmove | 1.26 s | 0.77 s |
| ksearch | 0.78 s | 0.31 s |
| kedit | 1.02 s | 0.87 s |
| ksort | 0.95 s | 0.88 s (its MOD of a 10-digit product needs more than 64 bits: the wide stack, correctly) |
| kstring | 0.51 s | 0.35 s |

**Tests, each against GnuCOBOL:** `tests/2002/hotfn` (the intrinsics),
`tests/free/inspfast` (table sweeps beside the general forms),
`tests/free/cmprm` (reference-modified compares).

## 2026-09-30, continued: loops, edited stores, UNSTRING, the index cache

- **Edited stores and fetches.** The DBT's `cob_put_num_x` hook declines
  a numeric-edited receiver, so every edited MOVE went through the
  guest-side dispatch in `cob_put_num_x_impl` before reaching the hooked
  `cob_put_edited`. libcob now keeps the locale word current
  (`cob_locale_word`: DECIMAL-POINT IS COMMA and the currency sign), and
  the register paths call `cob_put_edited` and `cob_get_edited`
  directly. A numeric MOVE to an edited item, and from one, takes this
  path. kedit: 0.74 s -> 0.59 s, no declines left.
- **Loop steps and bounds.** Every PERFORM VARYING and SEARCH step is
  `ADD 1`: the hot store now adds a literal that fits an immediate with
  `addi`, instead of spilling it to SLOT_A and reloading it. A hot
  compare against a constant loads the constant into the register
  directly. The serial SEARCH's fixed bound is one `slt` against it.
- **Short equality compares** (at most 16 bytes, native collating
  sequence) are the xor of word, halfword and byte chunks; unaligned
  loads are SLOW-32's by ruling. No call for a serial SEARCH's key.
- **UNSTRING with one delimiter of one byte** is a plain scan. kstring:
  0.31 s -> 0.25 s.
- **The indexed files' page cache.** btree.h found a page by a
  direct-mapped guess and fell back to a scan of the whole cache on
  every miss, and it chose a victim by a least-recently-used scan, also
  over the whole cache.
  - It is now an exact chained hash, where a miss is an empty bucket,
    and CLOCK's second chance for the victim.
  - `tests/bt_test`'s six shapes pass under a 16-page cache.
  - kidx: 0.48 s -> 0.39 s, and the files it writes are byte-identical.
- **`-fno-hot-arith`** now turns the peepholes above off too, so the
  Open Systems suite still compiles byte-identical to the baseline with
  it.

## 2026-10-01: after the front-end pass -- where the time is now

A measurement round before any change, on the nine kernels of
`bench/vs` and on jerm (majesty's date functions, 400,001 lines), under
the DBT on the development machine.

**Method.** `bench/prof.sh` builds a program against a libcob whose
every function is labelled, runs it under the reference interpreter
with `-p`, and `bench/prof.py` attributes the instructions: the
program's own generated code, libcob, the C library -- and, apart, what
the DBT runs natively (the hooked numeric and edit kernels, mem*).
What is left is "guest-only": the code the DBT translates. Wall time is
`slow32-dbt` itself, the median of five; `-H` (hooks off) shows what
the hooks cover -- kedit runs in 567 ms with them and 1,533 ms without,
so its edit routines are native, whatever a profile by name says. The
guest-only instructions become milliseconds at 8.7 billion a second,
ksearch's rate (it is all generated code).

| workload | wall | generated | guest libcob | native and I/O | largest guest runtime routines |
|---|---|---|---|---|---|
| karith | 484 ms | 20% | 15% | 64% | ndiv_core |
| kmove | 584 ms | 25% | 33% | 39% | ndiv_core, cob_move, cob_move_alnum |
| kedit | 567 ms | 19% | 0% | 81% | (all in the edit hooks) |
| kstring | 241 ms | 11% | 69% | 20% | cob_inspect_run, cob_unstr_into, cob_inspect_convert |
| ksearch | 256 ms | 99% | 0% | 1% | |
| kseq | 287 ms | 16% | 25% | 49% | ndiv_core, fread, fwrite |
| kidx | 385 ms | 2% | 31% | 65% | bt_pin, bt_descend |
| ksort | 945 ms | 2% | 77% | 15% | cob_wget, w_fit, cob_wput_x (the wide stack) |
| kreport | 340 ms | 5% | 44% | 44% | xs_merge_sort, w_fit, ndiv_core |
| jerm | 423 ms | 26% | 27% | 45% | cob_act_enter, cob_perform_enter/leave |

What it says:

- **Generated code is a fifth to a quarter of the time** where
  arithmetic and moves dominate, and all of it only in ksearch (a
  serial SEARCH and a binary one, a million times: about 2,200
  instructions an iteration). Laying loops out better, or keeping a
  loop item in a register, moves that share and no other.
- **The largest share is inside the hooks**: the numeric fetches and
  stores themselves, native as they are. A statement fetches each
  operand and stores each result; the way to spend less there is to
  fetch and store less often -- a value an earlier statement left, used
  again -- which is a data-flow question (what may overlap what), not a
  layout one.
- **One runtime routine stood out across kernels**: ndiv_core, the
  decimal division, a quarter to a third of the guest-only instructions
  in karith, kmove and kseq, each of which divides once an iteration.
- The out-of-line PERFORM's push and exit calls do not show in these
  workloads at all (their loops are inline); jerm's cost of that kind
  is function activation (cob_act_enter and cob_act_leave, 12% of its
  time) and cob_perform_enter and cob_perform_leave (5%).

**Division stops at the digits the receivers keep.** ndiv_core makes
the quotient a fraction digit at a time, to the operands' larger scale
plus six guard digits and never fewer than nine, and the store then
cuts it to the receiver's scale. The register path knows the receivers:
`cob_xdivn` takes the most fraction digits any of them keeps (one more
for a ROUNDED one) and makes no more -- each digit is exact, so the
store gets the same digits. Output byte-identical; karith 503 ms ->
439, kmove 605 -> 501, kseq 295 -> 265. What is left of ndiv_core in
karith is DIVIDE ... GIVING ... REMAINDER on packed items, which takes
the stack and divides twice.

**Moves the compiler can count.** kmove's loop called the runtime's
MOVE three times for what are byte copies: a four-byte part of an item
to PIC X(4), a one-byte part to PIC X, a 44-byte group to PIC X(44).
A group sender went to cob_move_alnum whatever the sizes, and any
reference-modified side sent the move through cob_move's dispatch. Now
a group to an item no longer than it, and a part to or from an
alphanumeric item or another part with both lengths written and the
sender as long as the receiver or longer, is a copy of the receiver's
length (emit_copy_fixed: inline bytes, or memcpy, which the DBT runs
natively). A shorter sender (padding), a JUSTIFIED or numeric receiver,
a computed length still go to the runtime. kmove 484 ms -> 379; over
the snapshot's programs 6,453 of 39,814 runtime move calls became
copies. `-fno-hot-arith` keeps the general path, and the two print the
same (tests/free/movefixed, which GnuCOBOL agrees with).

**ksort is its generator, not its sort**: five statements a record on
the wide stack, because `seed` is PIC 9(18) and `seed * 1103515245`
could pass 64 bits by its picture -- it never does. The register path
takes a statement only when the pictures prove every intermediate fits;
the plan's answer (docs/plans/performance.md) is the path the integer
one already has for a word: compute in 64 bits, test for overflow, and
fall to the wide stack only when it happens.

**Checked 64-bit arithmetic.**  A COMPUTE whose pictures do not prove
that every intermediate fits in 64 bits is now computed in 64 bits all
the same, with each operation's inputs tested as it runs, and the
wide stack's code for the statement kept behind the tests.  A test is
on magnitude in bits ("fit N": -2^N <= v < 2^N, four or six
instructions): before a product with a literal, or a scaling by a power
of ten, the other operand fits 62 less the constant's bits; before a
product of two items each fits 30; before a sum that could pass
9*10^18, each side that could be above 2^62 fits 62.  A test that the
pictures make unnecessary is not emitted, and a statement none of whose
operations needs one is the unchecked path it always was.  All the
tests come before anything is stored, and addition, subtraction and
multiplication are exact on both paths, so the two store the same
value; a division inside such a statement, a SIZE ERROR phrase, a
floating operand or a receiver of more than 18 digits leaves the
statement on the stack as before.  An eight-byte signed binary item
without truncation (COMP-5) is now a leaf of this path as well: any
64-bit value, tested where it is used.

ksort 972 ms -> 475, the same output: its generator's five statements a
record now run in registers, and no test in them ever fails.  The other
kernels and jerm are unchanged by it (they had no such statement).
Over the snapshot's programs 21 changed.

How it was checked.  `tests/gen/gen-checked.py` writes such statements
-- thirteen shapes over DISPLAY, BINARY, PACKED-DECIMAL and COMP-5
items of up to 18 digits, with values on both sides of every test: at
2^30, 2^31 and 2^62 and either side of them, and the largest the
pictures hold -- and `GEN=checked tests/gen/run-self.sh` runs each
program through the compiler before the change and after: 150 programs
of 40 statements, the same bytes.  The check itself was tested by
breaking the compiler five ways: a test that never branches (32 of 60
programs differ), the item-product test loosened (3), the scaling test
(15), the literal-product test (9), the sum test (3).  The last two
found nothing at first: the generator had no product of a large item
with a large literal summed with another, and gained a shape for it.
`run-self.sh` also counted a program neither compiler would build as an
agreement; it is a failure now.  `tests/wide-differential.sh`: 800
statements agree with GnuCOBOL.  CCVS-85: 348 programs, the same report
before and after.  `tests/free/checked64` holds the edges by hand, and
prints the same with `-fno-hot-arith` and under GnuCOBOL.

**The sign of a value truncated to zero.**  The first run of the
generated programs differed in one place, and the difference was older
than the change: -4611686018427387904 stored to a signed three-digit
DISPLAY item was `000` with a negative sign by the register path's
store and with a positive one by the wide stack's.  The store keeps
the low-order digits and the sign of the value (2023 14.9.25.4, the
MOVE rules: a signed receiver represents the value's sign), which is what
the narrow store did and what GnuCOBOL does; the wide store cleared the
sign when the kept digits were zero, and no longer does.  DISPLAY of a
signed zoned item whose digits are zero prints `+`, as GnuCOBOL prints
it, whichever sign the item holds.  `tests/free/negzero`.
