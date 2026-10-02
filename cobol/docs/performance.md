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
karith is that one division, `i * 3.25 / 8` a statement. (This note
first blamed karith's DIVIDE ... GIVING ... REMAINDER: wrong -- its
items are COMP, and the integer register path already takes it, as it
takes jerm's. No workload here has that statement on packed or DISPLAY
items, so it is no longer on the plan.)

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

## 2026-10-01: the batch itself -- where a real program's time goes

The kernels are stand-ins. The program they stand in for is majesty's
month-end batch: 28 runs of 26 programs, 2.0 s under the DBT -- and
1.2 s of it is one program, csv2fw, which turns the exported CSV files
into fixed-width ones a byte at a time: READ a one-byte record, a state
table, reference modification by a computed position, WRITE a one-byte
record. Its profile is not the kernels':

| what | share of the guest-only instructions |
|---|---|
| one COMPUTE on the wide stack (`v = v * 10 + NUMVAL(a digit)`, `v` an 18-digit item), 230,000 times | 25% |
| positions through the stack: cob_push, cob_pop_int, cob_load_int | 20% |
| the byte files: cob_read, cob_write and the C library under them | 21% |
| generated code | 18% |
| out-of-line PERFORM: cob_perform_push, cob_perform_exit | 6% |

and behind the second line, 24.8 million fetches through the numeric
hook, native but each a crossing.

**The tool grew two things for this.** `bench/prof.py` now prints how
often each routine was entered and what a call costs (the count at its
first instruction), and with `-fprofile-lines` -- a compiler option that
puts a global label where each statement's code begins -- `LINES=n
bench/prof.sh` lists the source lines whose own code ran most and how
many times each was begun. That is how one line of 896 was found to be
a quarter of the program.

**Positions in registers.** A subscript, a leftmost position or a
length that is an expression went through the runtime's stack: each
operand pushed (a call, and inside it a fetch through the hook), the
operations called, the value popped as an integer -- for `t(i + 1)`,
for `x(p:1)`, for a start that is one COMP item. And a subscript that
is an unsigned DISPLAY item was a call to cob_load_int, though a
condition on the same item already read its digits in line. Now an
expression over integer items and literals whose every intermediate
fits a word is computed by the integer register path where the
position is used (pos_reg_ok, arith_reg.h): the same value the stack
handed to cob_pop_int, since both are exact. What stays with the stack:
a packed or a signed DISPLAY operand, a decimal place, a division, an
operand that is itself subscripted (it would need the register the
outer reference's offset is in), and everything when
EC-DATA-INCOMPATIBLE is checked. A DISPLAY subscript's digits are read
in line.

csv2fw 1.20 s -> 0.90 under the DBT, its output files the same bytes;
the kernels are unchanged (their subscripts were COMP items already).

How it was checked. `tests/gen/gen-pos.py` writes references of every
such kind -- an item, a table, a table of two dimensions, a numeric
table; the position an expression over items of sixteen usages and
pictures on both sides of the line; a part of a subscripted element; an
operand itself subscripted -- each position chosen first and its
operands' values worked back from it, so every one is in range. 100
programs through the compiler before and after (`GEN=pos
tests/gen/run-self.sh`): the same. Four mutants of the change, all
caught: the value off by one (20 of 20 programs), the test for a
subscripted operand dropped (29 of 30), the pending position forgotten
across an element's subscripts (10 of 30), the DISPLAY subscript off by
one (20 of 20). 60 programs against GnuCOBOL (`GEN=pos
tests/gen/run-gen.sh`; Gate 7 now runs 40): agree -- once the generator
was told to keep intermediates at zero or above, because GnuCOBOL does
not: `x(2:a - b + c)` with 8, 9 and 2, `b` an unsigned BINARY item, is
a part of length 1, and GnuCOBOL computes the difference unsigned, so
the part runs to the end of the receiver; as a leftmost position or a
subscript the same expression is an invalid address and the program
dies (tests/2002/refmodneg, refmodnegp; docs/oracles.md). The old and
the new compiler agree on those too. CCVS-85: the same report.

**A number read a digit at a time.** The quarter of csv2fw that was one
statement: `v = v * 10 + FUNCTION NUMVAL(t(p:1))`, `v` an 18-digit
item. NUMVAL's value can be anything -- a fraction, a sign -- so its
result was a wide one and the statement the wide stack's, 8,000
instructions for a multiply and an add. NUMVAL of one character is a
leaf of the checked arithmetic now: a digit is its value, in line; any
other character sends the statement to the stack's code, which judges
it as before. With the product's test (`v` fits 58 bits) the statement
is ninety instructions and two crossings. csv2fw 0.90 s -> 0.78.
`tests/gen/gen-checked.py` gained the shape, with characters that are
not digits one time in five (three mutants caught: the test dropped,
the digit off by one, the scale wrong); `tests/free/numvaldigit` reads
numbers of one to eighteen digits into binary, packed and DISPLAY
items, and GnuCOBOL agrees.

**The byte files.** READ of a fixed-length record went to the C
library's fread for each record, a hundred instructions before a byte
moves, and WRITE and READ both ran their whole prologue -- seventeen
saved registers, for the print file's carriage and the variable
records' code that share the function -- whichever path they took. Now
cob_read and cob_write are small entries: a sequential file open for
input is read through the runtime's own block buffer (the one the
line-sequential read has), and when the buffer holds the record the
entry moves it and returns; a fixed-length record to a file open for
output goes from the entry straight to fwrite. Everything else -- the
refill, the end, a short last record, every other organization -- is
the rest, out of line, as it was. kseq 324 ms -> 237; csv2fw's READ and
WRITE about half what they were. A line-sequential WRITE pays one call
more than before (jerm: a half of one percent). `tests/free/seqblock`
is the check: records of seven bytes, which straddle every refill of an
8,192-byte block; the same file read ten bytes at a time, with a short
last record (04), the end (10) and a READ past it (46); one byte at a
time; EXTEND and the file read again. GnuCOBOL agrees, and three
mutants of the buffer are caught by it (the carry at a refill dropped;
a short record taken for a whole one; the entry ignoring how much is
left).

**PERFORM.** The runtime keeps the PERFORMs under way as frames, and
both ends searched them: cob_perform_push looked through the
activation's frames for the range already being performed (a GO TO out
of a range abandons its frame when the range is performed again), and
the code at the end of every paragraph called cob_perform_exit, which
looked through them for that paragraph. Now each paragraph and section
has a cell -- a word of the program's own -- holding the place of the
frame waiting on its exit, and each frame keeps what the cell held
before it. "Is this exit being performed" is one load: the end of a
paragraph reads its cell in line and calls only when it is not zero;
the push and the exit go to their frame without a search. The frames'
rules are what they were -- what a GO TO abandons, an exit nobody waits
on falling through, each activation's frames its own -- because the
cells are only an index into the same stack, unwound with it.

They are not what the kernels' loops spend time on (those are inline
PERFORMs); csv2fw performs five million times, and its push went from
60 instructions to 43 and its exit from 20 a paragraph to 4 where
nothing waits. With the files, csv2fw 0.78 s -> 0.68.

The check is `tests/gen/gen-perf.py`: programs that do nothing but
PERFORM, GO TO in and out of what is being performed, fall through
exits, call two contained programs and a second program in the file
that do the same and leave from the middle, and call themselves; every
paragraph counts a step and the run stops at 600. The trace through the
compiler and runtime before the change and after: 60 programs the same
(`GEN=perf tests/gen/run-self.sh`, which now builds the runtime of the
old revision too -- the two compilers ask different things of it).
Seven mutants, all caught: the exit not restoring the cells above it
(18 of 40 programs), the push not abandoning an active range (4), the
return of a called program not unwinding (7), another activation's
frame taken for this one's at the exit (3) and at the push (8), the
cells too few (30), one array of cells for the whole file (13). The
last two are there because of a mistake: the first version kept one
array for the file, sized by the last program's paragraphs -- and
paragraph numbers begin again at each program and are shared by
contained programs that are siblings. The generator had one contained
program, smaller than its parent, and saw nothing; CCVS-85's IC module
(ten programs that would not run) did. The generator now has what
would have shown it, and the compiler checks every cell it names
against its program's count.

csv2fw after the day's work: 1.20 s -> 0.68; the batch 2.0 s -> 1.56.
Its guest instructions went from 7.4 to about 3.9 thousand million.
What is left, by the same profile: the program's own code (a third --
its COMP items are big-endian, twelve instructions a load); fwrite in
the C library (91 instructions a byte); FUNCTION MIN in a length, still
the wide stack's (half a million times); a reference-modified move
through two descriptors; positions whose operand is itself subscripted.

**MIN and MAX in the register trees.** `t(1:FUNCTION MIN(n, 4096))`
keeps a length inside its item, and MIN's result is a wide one: the
length was the wide stack's, a thousand instructions, half a million
times. FUNCTION MAX and MIN of integers are nodes of the integer tree
now (keep one operand unless the other is the greater, or the less),
and of the decimal tree, where the two are aligned to one scale first
and compared as 64-bit values; an argument that is a table's ALL is
left to the runtime. csv2fw 0.68 s -> 0.55. `gen-pos.py` and
`gen-checked.py` have the shapes; three mutants caught (the two
exchanged, in each tree; the low words compared as signed).

**A part moved to a part.** `MOVE a(p:n) TO b(q:m)` with a computed or
omitted length built a descriptor for each part and called the general
MOVE, which looked at the two descriptors and called the alphanumeric
move. Now the compiler calls the alphanumeric move itself, with the
lengths; a computed length is checked as its descriptor's was (the
start and the length inside the item, or the run stops) by a routine
that returns the length and builds nothing. Three mutants caught.

**A position whose operand is subscripted.** `x(n(i) + 1:len(k))`
stayed with the stack: the operand's own address needs the register
the outer reference's offset is in. Such a position is now computed
before that offset begins, into a frame slot, and taken from there
(the stack's push was at the same place, for the same reason). Two
mutants caught.

**A literal moved to an item.** `MOVE 1 TO X` stores the same bytes
every time, and went through the numeric store every time -- a call
across the hook, about ten nanoseconds, 3.9 million times in csv2fw.
What the bytes are is the store's business (its truncation, its sign,
the usage), and the store is a kernel, `libcob/kern.h`, a file written
to be compiled wherever its answer is wanted. So the compiler includes
it too, runs it on the literal while compiling, and copies the bytes it
left: for any descriptor the kernel takes, a picture without P, a
literal of up to 18 digits. The same bytes by construction, and
`tests/kern-differential.sh` holds the guest's kernel to the host's.
`tests/gen/gen-lit.py` moves literals that fit and that do not into
items of every usage and prints every byte: 60 programs through the
compiler before and after, the same; three mutants caught. csv2fw
0.48 s -> 0.42.

A compiler bug met on the way, older than the day: `&g_desc[sym_desc(s)]`
reads the table's address and calls a function that may move the table,
in an order C does not fix. The new move tripped it (the harness's
exception-sites gate: the compiler crashed on one statement); two older
places had the same expression. All three take the index first now, and
the compiler built with the address and undefined-behavior sanitizers
compiles every test, the exception sites, majesty's sources and
generated programs clean: `tests/sanitize.sh`, 670 programs in a few
seconds, the harness's Gate 8 (it reports the old bug when the old
expression is put back).

**Where it stands.** csv2fw 1.20 s -> 0.42, and the batch 2.0 s ->
1.2; csv2fw's instructions outside the hooks from 7.4 thousand million
to under 3. What is left of it is the program's own code (half), fwrite
in the C library, PERFORM's two calls, and the class test of one
character. The kernels and jerm, this morning and now, under the DBT:

| workload | morning | now | what moved it |
|---|---|---|---|
| karith | 506 ms | 434 | division to the digits kept |
| kmove | 609 | 390 | division; moves that are copies |
| kedit | 579 | 573 | |
| kstring | 249 | 249 | |
| ksearch | 265 | 264 | |
| kseq | 294 | 219 | division; READ and WRITE as small entries |
| kidx | 393 | 386 | |
| ksort | 975 | 475 | checked 64-bit arithmetic |
| kreport | 350 | 328 | |
| jerm | 438 | 440 | (a line-sequential WRITE is one call longer) |
| csv2fw | 1,200 | 420 | everything from "the batch itself" down |

## 2026-10-01, last: the C library's short entries

What was left of csv2fw outside its own code was fwrite: 91
instructions for each byte the program writes, in `runtime/stdio.c`,
every clang-built program's library. fwrite, fread and fputc are now
short entries in front of their general routines (as fgetc was): a byte
into a buffered stream with room is 28 instructions (`runtime/ISSUES.md`
14 has the design and its tests). csv2fw 0.42 s -> 0.37; jerm, which
writes 400,000 lines, gets back the call it lost to the READ and WRITE
entries.

Three defects of the library were found by the test written for it and
fixed with it (`runtime/ISSUES.md` 15 to 17): output directly after
input that met end-of-file was lost; fseek from the current position
counted from the buffer's read-ahead; ftell on an append stream counted
from zero. None was on a path a COBOL program takes.

It is a platform change, so the platform's gates ran: the regression
suite (97), the cross-engine differential, SQLite's acceptance,
Fortran's suite, the dBASE interpreter over majesty's reports (the same
bytes out), mdfix's parity harness, and everything here.

The self-hosted libc, which the kit's own tools link, has no stdio
buffer at all (`runtime/ISSUES.md` 18): not COBOL's library, but the
same opening for the kit's compiler, assembler and linker.

## 2026-10-02: PERFORM's push and exit, written out

Measured again first, on the day's machine and the day's C library
(which changed underneath: `runtime/ISSUES.md` 21 to 27; csv2fw built
against the library before and after times the same, 414 ms and 413).
The numbers below are from alternating runs of the two binaries, seven
or nine of each, medians; this machine was about a tenth slower than
when the table above was taken, so they are to be read against each
other and not against it.

csv2fw's profile, of the instructions that are not inside a hook:
generated code 56%, cob_write 9%, **cob_perform_push 9% and
cob_perform_exit 6%**, cob_read 6%, fwrite 5%.  The push and the exit
were made constant on 2026-10-01 -- a cell for each exit -- but stayed
43 and 30 instructions, 5.35 million times.

Most of that was not the work.  The push is three stores and two
counts; compiled, it saved five registers and the return address for a
`realloc` it reaches once in a run, and formed each of the stack's four
words' addresses separately.  The C compiler does that however the C is
arranged: split into an entry and a rest, the entry still had a frame
(the SLOW-32 backend makes no tail call, so the entry's call to the rest
is a call, and a function with a call saves its return address) and
still spent three instructions on every word (it takes a static
structure apart into four variables).  28 and 25.

So the two entries are written out, in `libcob.c` beside the general
routines they stand in front of: no frame, the stack's words at offsets
from one address, a frame of sixteen bytes so that its place is a
shift, and the general routine a jump away with the arguments where
they were.  **17 and 16 instructions.**  csv2fw 421 ms -> 391, a
twelfth of its instructions gone; jerm 428 -> 423; the nine kernels,
which perform nothing out of line, unchanged; every output the same
bytes.

Putting the same instructions in line at each PERFORM and each
paragraph's end would save the call and the return and nothing else --
the address of the stack's words has to be formed either way -- for
some sixty bytes at every site.  Not done.

`tests/gen/run-self.sh` with `GEN=perf`: 400 generated programs of
PERFORMs left by GO TO, performed again, called through and recursed
into, the same before and after.  Eight faults put into the two entries
by hand: seven caught by 150 of those programs each.  The eighth -- the
test for a full stack -- passed them all, because nothing filled it:
256 frames is more than any of them leaves waiting.
`tests/2002/performdeep` leaves 1,400 (a recursive program, two ranges
under way in each of 700 activations), and that fault stops it.

**What this says about the rest.**  cob_write (54) and cob_read (55)
are entries of the same kind with the same frame on every path, and
fwrite (30) under one of them; a WRITE of one byte is 84 instructions
of library.  The frame is a few of them.  More are tests of what was
settled when the file was opened -- its organization, its mode, whether
it has a LINAGE or a code set -- asked again for every record, and the
call into the C library to store a byte in a buffer.  That is next.

And it says the backend wants tail calls: with them an entry that ends
in "otherwise, the general routine" has no call in it and no frame, in
C.

## 2026-10-02, later: the backend makes tail calls

The last paragraph, done.  The SLOW-32 LLVM backend (`llvm-backend/`)
now lowers a call in tail position to a jump -- `jal r0, sym`, or `jalr
r0, r2, 0` through a pointer -- after the epilogue, when every argument
is in a register, nothing is passed by value or returned through a
hidden pointer, and the caller is not variadic.  A function whose only
call is a tail call has no frame at all, which is what every "short
path, otherwise the general routine" entry in the C library and in
libcob wanted: fwrite's is 26 instructions where it was 30, with
nothing rearranged.

With no change here: csv2fw 351 ms -> 338 (alternating runs, the
libraries before and after), the same bytes out.  libcob is 0.5% larger
-- a tail call carries its own copy of the epilogue.

It also changes what the next step should be.  READ and WRITE of a
fixed-length record can be entries in C now: one flag settled at OPEN,
the byte stored in the stream's buffer, no call on the short path and
so no frame.  The two PERFORM entries stay written out for the present:
in C they are 24 and 22 instructions to the 17 and 16, because the
backend forms the address of each word of a global structure
separately -- three instructions a word, where one address and four
offsets would do.  That is the next thing the backend wants
(`llvm-backend/SLOW32/STATUS.md`, Future Opportunities).

`regression/tests/feature-tail-call` runs it: a million activations of
mutual recursion on a stack that holds a few thousand, direct and
through a pointer; eight arguments out of a frame past the 12-bit
offset; a pointer kept across a call and then jumped through.  The
regression suite compiled everything at -O0, where no tail call is
formed; a test can now ask for a level (`clang-opt`).

## 2026-10-02, last: READ and WRITE ask once

csv2fw reads its input a character at a time and writes its output the
same way: 2.6 million READs and 4.4 million WRITEs of a one-byte record,
16% of its instructions in `cob_read`, `cob_write` and the `fwrite`
behind the second.  A READ was 55 instructions and a WRITE 52 and
`fwrite`'s 26, and most of each was questions whose answers do not
change between OPEN and CLOSE: the organization, the open mode,
variable records, REVERSED, CODE-SET, LINAGE, what kind of stream the C
library gave.

They are asked once.  The first record of a file goes the whole way
round; what that finds is left in four flag bytes at the end of the
file's block (a word the compiler now emits, the runtime's alone), and
CLOSE takes them away.  After that

- `cob_read`, `cob_write`: a one-byte record.  One flag, one test that
  the buffer has the byte or the room, the byte, the position, the
  status.  No call, so -- the backend making tail calls -- no frame:
  28 instructions and 27.
- `cob_read_n`, `cob_write_n`: any other fixed length; the same with a
  `memcpy`, and the small frame that needs.
- `cob_read_rest`, `cob_write_rest`: everything else, and whatever the
  buffer cannot settle -- filling it, the end of the file, a short last
  record, a stream that has to be emptied.

Each is a jump from the one before with its arguments where they were.

A WRITE on the short path stores into the C library's own stream
buffer.  `<stdio.h>` has three inlines for that (`__s32_out_plain`,
`__s32_out_room`, `__s32_out_byte`; `runtime/include/stdio.h`), with
`fwrite`'s own tests: the same bytes at the same places, and when there
is no room the record goes through `fwrite`, which empties the buffer
at the same record it always did -- so a full device is reported by the
same WRITE as before.  Built against some other C library, every record
goes through `fwrite`.

The status every I-O statement leaves for EC-I-O checking was two
characters in two separate globals to the compiler's eye -- seven
instructions to say "00".  It is one word that is zero for 00: three.

csv2fw 333 ms -> 290 (alternating runs; the same bytes out); its
instructions outside the DBT's native routines 2.33 G -> 2.03 G.  kseq,
whose records are longer, 208 ms -> 199.  The other kernels did not
move.  Where csv2fw is now:

    71.3%  the program's own code
     5.9%  cob_write            4,401,094 calls, 26 each
     4.7%  cob_perform_push     5,351,231 calls, 17 each
     4.2%  cob_perform_exit                      16 each
     3.6%  cob_read             2,635,661 calls, 27 each
     3.5%  cob_class              884,525 calls, 81 each
     2.1%  cob_refmod_len_chk   1,341,526 calls, 32 each

The runtime is 28% of it and no one routine is a tenth of that.  What
is left is the generated code: stage 3 and stage 4 of the plan.

### What the tests for it found

**A full device went unreported.**  `free/faultbyte` writes one-byte
records to a device that refuses the first write, and expected the
4,096th WRITE -- the one whose byte fills the stream's buffer -- to take
34.  All 5,000 took 00, and the file held 904 records.  Not the new
code: the C library's `fwrite` counted a request's bytes as written
before the send they were part of had failed, when the request ended
exactly at the end of the buffer (runtime ISSUES-28).  `free/faultwrite`
had 1,000-byte records, which run past the end and had always come back
short.  Repaired in `runtime/stdio.c`; the library's own test is
`regression/libc-tests/stdio_fault`.

**The runtime no longer built without LLVM.**  `cctool.sh` falls back
to the self-hosted stage08 cc where there is no clang.  The PERFORM
entries of this morning were a file-scope `asm` in libcob.c, which that
compiler does not have; they are `libcob/entries.s` now, appended to
the compiler's assembly as the hook thunks are.  And `esql.c` had not
compiled there since the PostgreSQL work: a local `host` under the
file's own `host` typedef, which stage08 cc read as a cast (selfhost
ISSUES-78; repaired in the compiler).  No gate ran that path.
`tests/selfhost-libcob.sh` does: the runtime built by the self-hosted
compiler into a directory of its own, and every program of the suite
run against it.

### Tests

`free/seqbyte`: more one-byte records than the buffers hold, written
and read, every byte checked, the FILE STATUS item set to something
else before each statement; the same bytes as nine-byte records, one
straddling every buffer and the last short; EXTEND; nine-byte records
out and bytes in; a file with no status item; and one connector closed
and opened the other way round, with the wrong statement in the middle
of a run of right ones (47, 48).  `2002/seqbyteec`: an invalid key
condition on another file between short-path statements, under EC-I-O
checking -- the status they leave must raise nothing.
`free/faultbyte`: the full device, one-byte records and records that
fill the buffer exactly.  `free/codesetrecs`: a file with a CODE-SET is
not on the short paths -- its second record is translated as its first
is.

Forty-eight mutants -- of the entries, the flags, the status word, the
three inlines, the library's repair, the compiler's extra word.
Thirty-seven are caught.  Six of those were not at first: a successful
statement's status is read only under EC-I-O-WARNING checking, which
the first `seqbyteec` did not turn on, and nothing wrote a second
record through a CODE-SET.  The eleven that survive change nothing a
program can see: the file position and the last record length, kept on
the short paths though nothing reads them for a file that is on those
paths (REWRITE needs I-O, REVERSED and variable records never get
there) -- six; the flags never set, which is only slower -- two; the
read flags left set at CLOSE, harmless because CLOSE empties the buffer
they guard -- two; a truncation test the count beside it implies -- one.
The position and the length stay: three instructions a record buy the
fields meaning what their comments say.

## 2026-10-02: a class test of characters

`x(i:1) IS NUMERIC` built a descriptor for the part and called the
runtime's general class test: 88 instructions and 81, to compare one
byte with two bounds.  csv2fw asks it once for each character of each
amount it scans.

A class condition (NUMERIC, ALPHABETIC, ALPHABETIC-LOWER, -UPPER) whose
operand is alphanumeric bytes -- an elementary alphanumeric item, or a
reference-modified part that is not national, bits or an
occurs-depending group's, the conditions of the direct alphanumeric
move -- has nothing for a descriptor to say.  One character is tested
where it stands: the byte, a subtraction and a compare for NUMERIC;
for the alphabetic classes the letter's range and the space.  More
than one, or a length known only when running, is `cob_class_bytes`
with the address and the length (the part's length checked as its
descriptor's was).  Everything else -- numeric items and their signs,
packed, national, BOOLEAN, a class of SPECIAL-NAMES -- is `cob_class`
as before.

csv2fw 302 ms -> 289 (alternating runs, the same bytes out).

`free/classbytes`: every byte value as a one-character item, as a part
of one, and at each position of a part of four and of a part of
computed length, counted into each class and out of it by the negated
condition -- 10, 53, 27, 27.  The harness compiles every program a
second time with the in-line forms off, so the two are compared on
each.  Twenty mutants of the emitted tests and of the runtime's loop,
all caught.

