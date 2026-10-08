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

## 2026-10-02: stage 3 begun -- a loop's items in registers

With the runtime's routines small, seven tenths of csv2fw is the
program's own code, and the code is verbose in one way above all: a
COMP item is loaded from storage wherever it is used -- its address and
a byte swap, six instructions -- and a loop loads its own item to test
it, to step it and wherever the body mentions it.  The loop that writes
csv2fw's output a byte at a time was 57 instructions a byte before the
WRITE's 27.

Inside an in-line PERFORM's loop, a binary item is now kept in a
callee-saved register (r14 to r17) as well as in storage
(`src/cobc/loopreg.h`).  A load is a copy; a store is the store and a
copy, the register made what a load of the bytes stored would give.
Storage stays current, so anything that reads the item some other way
reads it right, and nothing has to be done where the loop is left.
What has to be ruled out is anything that could change the item some
other way, behind the register.

**That is decided from the code, not from the statements.**  The loop's
lines are read once they are all there, following what each register
and frame word holds as far as "an address in this record": a store
through an address is a store into that record (two records never share
storage -- a redefinition has its subject's label, and an item reached
through a cell, LINKAGE or BASED or EXTERNAL, is an address nobody
knows); a call is what a table says its routine may store through, and
a routine not in the table may store anywhere; a jump to anything but a
label of the compiler's own, a jump through a register, an instruction
not listed -- anything may have happened.  An out-of-line PERFORM, a
CALL, a declarative are all of that last kind.  At a label reached only
from above, what is known is what every way in agrees on; at any other,
nothing.  An item none of it can touch gets a register, loaded once
where the loop is entered.

The emitters only leave marks in the line stream (`#@L`, `#@S`, never
written out): "these lines load this item", "these store it".  A mark is
a claim, checked against the lines under it before it is acted on, so a
wrong mark costs the rewrite and nothing else -- and the one way a
wrong mark could have cost more was found by the unit test, below.

The output loop is 41 instructions a byte.  csv2fw 291 ms -> 283 (the
same compiler with `-fno-loop-reg`, alternating; the same bytes out);
karith -3.6%, kmove -2.6%, kedit -1.9%, kseq -1.6%, the other kernels
unmoved.  It is where in-line loops over binary items are: five of
majesty's programs, a few of the X-COBOL ones, none of CCVS-85 or the
Open Systems suite, whose loops are performed paragraphs counted in
DISPLAY items.

### What checks it

- `tests/loopreg_test.c` (the harness's gate 1f): the reading itself,
  given lines written for it -- 104 checks.  Most of what it must refuse
  the compiler does not emit today: a register holding one address by
  one path and another by the other, a store through a register left
  from before a call, a label reached from below.  The first mutation
  run said so: twenty of forty mutants of the analysis survived every
  COBOL program there was.
- `tests/2002/loopitems`: each way of changing a loop's item behind its
  name, on the loop's second pass -- a redefinition, the group (moved to,
  initialized, one byte), a table over it, a performed paragraph, READ
  INTO, the record area, a FILE STATUS, ROUNDED, STRING's pointer, a
  BASED item and a BASED group at its address, a long element, a
  RELATIVE KEY, LINAGE-COUNTER -- and the stores the register follows.
- `tests/gen/gen-loop.py` through `tests/gen/run-flag.sh -fno-loop-reg`
  (the harness's gen/loop): generated loops of every form, nested, whose
  bodies do all of the above at random; the same compiler with the
  rewrite off is the oracle.
- `free/calleesaved`: C holds values in r14 to r17 across a call into a
  program whose loops use all four.
- Every program of the harness a second time under `-fno-hot-arith`,
  which leaves the rewrite out.

Forty-one mutants, all caught in the end.  The unit test found one
defect in the reading as first written: a marked store whose mark did
not hold was left alone -- rightly -- but not counted against its item,
which could then have been kept in a register that store went behind.

### What it is not yet

- DISPLAY integers (a loop counter `PIC 9(4)` is fifteen instructions to
  load): the same marks, when the store's contract is nailed down.
- The item left in its register and stored only where the loop is left.
- Straight-line code.  The same reading says where an item's value is
  still in hand from the statement before; that is stage 3's second
  step, and it does not need a loop.
- A loop whose body is a performed paragraph: the body is somewhere
  else, and may do anything.

`S32_LR_TRACE=1` makes the compiler say, for each loop, which items it
kept and which line refused the others.

## 2026-10-02: the batch, measured again -- and a correction

**A correction first.**  The batch totals on this page up to here (2.0
s, 1.56 s, 1.2 s) are sums taken by a wrapper that started an
interpreter before and after every run to read the clock, and the
second start was inside the interval: a constant of 15 to 25 ms a run,
about half a second a batch, that was never the emulator's.  The batch
was about 1.4 s when this round began, not 2.0.  A program's own
numbers (csv2fw's, the kernels') were always taken directly, by
alternating runs, and stand.  The wrapper now takes the time around the
emulator's process and nothing else.

Measured so, the month-end batch is **0.62 s** of emulation over 28
runs:

    csv2fw                       274 ms
    seven GL reports             46, 39, 33, 33, 25, 23, 22
    the other twenty             11 ms and less (an empty program is 2.5)

csv2fw is under half of it now, and the reports are next.  The largest
of them, 42 ms on its own, by the DBT's own account (`slow32-dbt -p`):

    translating the program      10 ms
    dispatch                      4 ms
    executing it                 27 ms

and by the profile of what it executes, outside the routines the DBT
runs natively: the SORT a third (the merge 23%, building keys 10%),
line sequential READ a quarter (finding the end of the line 17% --
`memchr`, 379 instructions a record, which is not one of the DBT's
native routines -- and the READ itself 7.5%), line sequential WRITE a
tenth, the program's own code a sixth.

What that says:

- A third of a 40 ms run is the DBT translating code it translated a
  minute ago for the last program: libcob is most of every program
  here.  Over 28 runs that is the largest single thing left in the
  batch -- and it is the DBT's, not the compiler's: translated code
  kept from one run to the next.
- In the runtime: line sequential READ and WRITE have no short path of
  the kind the fixed-length records got; `memchr` is a byte at a time.
  A few milliseconds a report.
- In the compiler: csv2fw's own code, 71% of it, which is the rest of
  stage 3 and then stage 4.

Each of the last two is a few percent of the batch.

## 2026-10-02: stage 3, the second step -- a value still in hand

The reading that decides a loop's registers follows addresses through
the code.  The same pass over a whole unit (`lr_unit`, at the unit's
end) follows one thing more: which item each of the four registers
holds.  An item loaded, or stored by its marked store, is held from
there.  It stops being held where something may store into it (the
loop pass's rules, but here they end a holding where there they refused
the item), at a label nothing is known at -- a paragraph, the top of a
loop, the place a PERFORM comes back to -- and where a loop that owns
the register begins.  At a label reached only from above it is held if
it is held by every way in.  A load of an item that is held is a copy.

Nothing is put in a register on speculation: the pass notes, for every
load that takes its item from a register, the loads and stores that put
it there, and only those get the instruction that does it.  When all
four registers are spoken for, the item wanted longest ago gives its up.

Unsigned DISPLAY integers are items now too, by their loads: the value
of `PIC 99` is its digits, seven instructions to work out, and a
program's state variables and subscripts are often that.  Their stores
are not marked -- what a store of digits leaves is the store's own
business -- so a DISPLAY item is held from a load until anything stores
into it.

csv2fw: 4.155 G instructions with none of stage 3, 4.075 G with the
loops' registers, 3.955 G with this -- and 285 ms, 275, 271.  The
kernels do not move but ksearch (-2.4%).  What changed more is where it
reaches: loads taken from registers in 127 of CCVS-85's 370 programs
and 107 of the Open Systems suite's 227, which the loop pass did not
touch at all, and 25 of majesty's 57.

In csv2fw's own hot paragraph it helps less than its share of loads
would suggest, and the reason is the shape of the code: the state is
read, then replaced by a table entry moved as two characters, then read
again by each WHEN -- so it is worked out from its digits twice a
character where it was five times, and a compiler that knew the move
was of a number would not work it out at all.

### What checks it

- `tests/loopreg_test.c`: 40 more cases, of what each mark comes out as
  -- taken from a register, left in one and taken later, left in one for
  nothing, neither -- over stores, calls, joins where the ways in agree
  and where they do not, loop tops, loops that own registers, five items
  for four registers.  144 checks.
- `tests/2002/heldvalues`: an item read, changed behind its name by each
  of the ways `loopitems` uses, and read again; one side of an IF
  changing it and the other not; DISPLAY items changed as characters.
  GnuCOBOL agrees with every line.
- `tests/gen/gen-loop.py` grew straight runs between and inside its
  loops, IFs whose sides differ, and DISPLAY items; the harness runs it
  with `-fno-loop-reg` (everything off) and, on other seeds, with
  `-fno-avail-reg` (this step off).
- Fifty-four mutants of the analysis as it now stands: fifty-three
  caught; the one that survived removed a line that did nothing, and
  the line is gone.


## 2026-10-02, evening: numbers that stand alone, written the machine's way

The first step of the road to HIR that changes code
(`docs/plans/census.md`, step 2; cobol ISSUES-123).  The census found
that half of what the statements name stands alone.  A number among
those whose every use is a use of its number -- no statement takes its
bytes -- is now stored as a binary integer in the machine's byte
order, whatever its entry says: `PIC 9(4)` is two bytes of binary, not
four digits; `PIC S9(7)V99 COMP-3` is a word holding hundredths; a COMP
item is not swapped on its way in and out.  The picture still limits
it, rounds it and places its point.  An item whose own bytes are too
few for that (a packed one of eleven digits has six, and wants eight)
gets a cell outside its record; an element of a table is changed where
it is, every occurrence.  7,609 items in the corpora, 25.6% of the
references.

| | as written | the machine's way | |
|---|---:|---:|---:|
| kmove | 381 ms | 271 ms | -29% |
| karith | 419 ms | 336 ms | -20% |
| kseq | 205 ms | 167 ms | -18% |
| kedit | 573 ms | 484 ms | -16% |
| kstring | 258 ms | 233 ms | -10% |
| kreport | 321 ms | 305 ms | -5% |
| ksearch | 258 ms | 249 ms | -3% |
| kidx | 393 ms | 390 ms | -1% |
| ksort | 490 ms | 489 ms | 0% |
| csv2fw | 279 ms | 255 ms | -9% |
| majesty's batch, 28 runs | 621 ms | 583 ms | -6% |

It came in two parts, and the first was the smaller.  Integers alone
gave the kernels 0.3 to 3.8% and csv2fw 2% (3.955 G instructions to
3.873 G): stage 3 had already put the hot integers in registers, and
what was left of an integer's cost was its store.  Decimals are the
rest: karith's COMP-3 and signed DISPLAY items, every one standing
alone, 11.16 G instructions to 8.31 G.

The batch moves less than the kernels.  118 of majesty's items change,
19.4% of its references -- its amounts, packed items of eleven digits,
once they could leave their records and a report's SOURCE was taken
for the use of a number that it is; and csv2fw's tables, the COMP
table its hot loop walks no longer swapped at each access (3.955 G
instructions to 3.794 G).  The rest are held by partners, named groups
and LINKAGE; and what is left of the batch's time is csv2fw's text and
each short run's translation.

What is then left in karith is the arithmetic: 4,155 instructions a
pass for seven statements, pushed and popped through the runtime's
stack of 64-bit numbers.  That is not the data's to fix.

`-fno-native-items` turns it off; `S32_NATIVE_TRACE=1` lists the items
changed; `S32_CEN_TRACE=1` (with the switch off) says what pinned each
item that was not, and at which statement; `tests/census.sh` counts
them over every corpus, with what kept the others.

### How it knows

The compiler emits as it reads, and whether an item may change is
known only at the program's end.  The compiler forks: the child
compiles the program as written with the census on and sends back the
items; the parent changes them before any statement is compiled.

An item may change when it stands alone (the census), starts as a
number, and every use of it is a use of its number.  The last is
decided where the item's address is formed: followed by a load or
store of its value, or by a runtime routine that takes it as a number,
it is a use of the number; followed by anything else it is a use of
the bytes.  Two items of one description that are copied or compared
byte for byte change together or not at all.

### What the tests for it found

Besides its own first mistake (an item laid over an alphanumeric one
starts as spaces, which every gate passed and the generator's first
forty programs did not), three defects that were there before it, each
found because one program compiled two ways disagreed:

- `DIVIDE D INTO N GIVING D REMAINDER R`: the stack's path worked the
  remainder out after storing the quotient over the divisor, and left
  0 where 10 / 4 leaves 2.  `tests/free/divremgiving`.
- DISPLAY of a packed or binary number under DECIMAL-POINT IS COMMA
  showed a period.  `tests/free/dpcommausage`.
- `ADD P TO P R`: the in-line decimal add read P again for R, after
  storing P; the other paths add P as it was (X3.23-1985 6.4.6).
  `tests/free/hotdec` had the wrong line.

GnuCOBOL agrees with the first two tests, and with the old line of the
third: it reads the operand again (`docs/oracles.md`).

### What checks it

- `-fno-native-items` is the oracle: `tests/gen/gen-native.py`,
  300 programs the same both ways, 60 of them in the harness
  (gen/native); the other eleven generators, forty programs each, the
  same.
- `tests/census_test.c`, harness gate 1h: the address rule on events
  written for it, 38 checks.
- Mutants: cobol ISSUES-123.
- Off, it changes nothing but the repairs of those defects.

## 2026-10-08: the lock layer in front of the short entries

**Measured first.**  The month-end batch, each emulator run timed
around the DBT's process (28 runs): 616 ms, csv2fw 250 of them as the
batch runs it (concurrently with the C++ side; 215 alone), gl034 45,
gl038 42, gl035 40.  csv2fw was 0.17 s on 2026-10-03.  Built again with
the compiler and runtime of that day (a worktree at 74d17831) and run
alternately with today's under one DBT:

    74d17831   3,295,159,176 instructions   198 ms
    today      3,546,139,630                215 ms     +7.6%, +8.6%

**What it was.**  Queue item 39 put the record-lock layer in front of
`cob_read` and `cob_write`: the one-byte short entries of 2026-10-02
(28 and 27 instructions, no frame) became an inner function behind a
test of `lk_active`, a loop whose registers the compiler inlined into
the entry, and a clear of the statement's options -- 80 and 50
instructions a byte, with a 21-register prologue on every path.  The
profile said so at once (`cob_read 2,635,661 calls, 80 each`); the
differential against the old build said how much.

**The repair, in four steps, each counted** (csv2fw, instructions):

1. the one-byte path first in the entry, before the lock test:
   3,510,976,964 -- the entry still paid the frame the inlined loop
   wanted;
2. the lock loop in its own `noinline` function, the entry a tail call
   to it: 3,357,168,567 -- the entry now saves `lr` only, for the
   `memset` that cleared the options;
3. the options cleared field by field: 3,333,422,677 -- the entry has
   no frame, but tests `cob_io_opt.set` on every byte;
4. a READ or WRITE that carries lock phrases calls `cob_read_opt` /
   `cob_write_opt` (the compiler knows: it emitted `cob_io_set`), so
   the plain entries never ask: 3,300,881,285, 29 and 28 instructions.

Then the report programs: majesty's `SHARING WITH ALL OTHER` turns the
layer on for the whole run unit, and every READ and WRITE of every
file -- none with a lock mode -- went through the loop and `lk_after`
(359 instructions, 12 saved registers), `lk_before`.  The slow entries
now test `lk_locks_effective` first and tail-call the inner read or
write for a file the layer has nothing to do for, the loops a function
further down: gl033 -6.2%, gl045 -4.0%, gl034 -3.0%, gl043 -2.3%,
gl042 -2.2%, the rest -0.7 to -1.8%; the batch -1.0% beyond csv2fw.

    csv2fw, alternating with 74d17831's build:   198 ms  ->  176 ms

Twenty milliseconds under the old build at the same instruction count:
the DBT of 2026-10-06 (DBT-22's chaining) and the layout.

**Lesson.**  A runtime entry on the hot path has a contract that is
not written anywhere: frameless, no call before the fast return, a tail
call out.  The lock layer kept the semantics and broke the contract,
and the gates -- which count nothing -- said yes.  The fix is a
reading of `libcob.s` for the two entries (`cob_read:` must open with
the `fast_r1` test, no `addi sp`), which this page now records as the
check; a gate that counts csv2fw's instructions against a bound would
have caught it the same day.

### Where csv2fw's time goes now

Guest instructions (`bench/prof.sh` through the batch, so the program
reads majesty's data), 3.21 G: 68.6% run natively under the DBT -- the
memsets and memcpys and the numeric hooks -- and 1.01 G are translated:

    39.2%  the per-byte island (__isl_0)      2,724,017 calls, 145 each
    11.8%  cob_write                          4,401,094 calls,  26 each
    10.0%  the write loop's island (__isl_21)  4,401,094 calls,  22 each
     7.7%  the program's text                 4,632,217 calls,  16 each
     7.3%  cob_read                           2,635,661 calls,  27 each
     6.3%  __isl_9 (parse-amount)               845,690 calls,  75 each
     5.5%  __isl_11                             919,376 calls,  60 each
     1.8%  cob_refmod_len_chk                   564,328 calls,  32 each
     1.7%  cob_class_bytes                      335,284 calls,  50 each
     1.4%  cob_fn_integer_of_date                57,666 calls, 238 each

and the host's time, sampled (`sample` on the DBT, 106 samples): 80
in translated code, 15 in `memset`, 7 in the numeric hooks, 2 in
`memmove`.  The memsets are the program's: `MOVE SPACES TO
FLD-TEXT(k)` and then `MOVE OWN-TEXT(1:n) TO FLD-TEXT(k)` fill a
4096-byte item twice a field, 535,100 and 514,888 fills of 4 KB -- a
seventh of the run, and not the compiler's to remove (the first fill
is dead only when the second runs).  The hooks are 870,000 crossings
of a fetch or a store; stage 2's whole-operation hooks would halve
them.

The per-byte island is 1,254 HIR instructions in 190 blocks and runs
145 a byte.  Read for waste: every item is loaded from storage at each
use (149 `gaddr`, 82 loads for a handful of items -- `state`,
`f-seen`, `f-owned`, `byte-class` -- the optimizer does not keep a
loaded value across a store to another item); `state` is PIC 99
DISPLAY and is decoded (`and 15`, `mul 10`, `add`) at each of its
reads, because its partner `stt-next` is DISPLAY and the census keeps
the pair as written; a one-byte literal moved to an item is loaded
from its label (`gaddr .Lstr0; load; store`) where an immediate would
do; and the address of every global is a `lui` (205 of the island's
1,127 instructions), the same page over and over.  Those are the next
levers in the generated code, in that order of ease.

### The report programs

gl034 (the largest, 42 ms): 461 M instructions, 68.6% native (memchr
is a native routine of the DBT now, and counted so); of the 145 M
translated: the SORT's merge 19.9% and its key building 8.9%,
`cob_read_rest` 6.5% and `cob_write_rest` 5.8% (line sequential
records, 167 and 147 instructions each), the lock layer 12% before
today's fix, the program's own code 10.9%.  The SORT -- a key image
built per record, a merge comparing them -- is the one COBOL-shaped
routine here that would make a natural native hook; the rest is
small.

### Tools

`bench/emu-standin.py` stands in for the emulator in a script that
runs many programs (majesty's `batch.sh` takes `S32_EMU`): per run, the
DBT's process timed (`MODE=time`), or the instruction count under
`slow32-fast` (`MODE=count`), or one named program run under the
reference interpreter's profiler in the batch's own directory with the
batch's own arguments (`MODE=prof`), which is how the profiles above
were taken.  `bench/sites.py prog.s32x prog.prof` is the call-site
profile: every `jal` in the text with its count, the function it is
in and its source line, so a runtime routine's callers are told apart.
Islands carry a global alias under `-fprofile-lines` (`__isl_N`), so
`prof.py` tells them apart; and `memchr` is counted among the DBT's
native routines, as it has been since 282eb903.

## 2026-10-08: the SORT's run as a native hook

The report programs' profile pointed at one COBOL-shaped routine: the
merge sort over a run of released records, `xs_merge_sort`, 20% of
gl034's translated instructions, with a `memcmp` crossing to the host
for every comparison -- 55,826 records, some 900,000 comparisons.  The
user's framing: hot spots that are candidates for a DBT hook running
specialized native code, "making the processor more COBOL friendly".

`cob_sort_run(buf, esize, klen, n, order, tmp)` is the fourth hook
(docs/dbt-hooks.md step 4): the n entries of a run, each a normalized
key of klen bytes before its record, and the stable ascending order of
their indices.  The kernel in `libcob/kern.h` is a bottom-up merge with
an inline byte compare; compiled into libcob it is the guest's
reference (`cob_sort_run_impl`, which xsort.h now calls in place of its
own recursive merge -- the same order, as every stable sort gives), and
compiled into slow32-dbt it is the hook: the DBT resolves the three
guest ranges once and sorts the whole run in one crossing.  A run too
large for the guest's memory, misaligned index arrays, a key longer
than the entry: declined, and the guest sorts.  The key build stays in
the guest -- 230 instructions a record is under the cost of a crossing
that copies the key descriptors out.

Measured, the month-end batch timed per run, medians of three batches,
before and after rebuilding majesty's programs against the new libcob,
the same DBT:

    gl038    50.8 ms  ->  36.2      -29%
    gl036    34.5     ->  26.1      -24%
    gl034    46.5     ->  38.7      -17%
    gl035    38.0     ->  34.6       -9%
    batch   565.5     -> 498.0      -12%     (616 this morning, before the lock-layer fix)

Checks: `tests/kern-differential.sh` drives the routine over 40 random
runs with many equal keys under slow32-fast and slow32-dbt (the hook
called 40 times, declined 0), and a mutant of the host side (the key
one byte short) is caught -- the kernel being one source on both sides,
the differential tests the crossing, not the algorithm, which the
gates' SORT tests, CCVS's SM module and majesty's byte-identical reports
test.  Then the three engine differentials and every COBOL gate.

## 2026-10-08: loads known across blocks

The second item the user ordered after the hot-spot map: the island
reloads an item at every use.  The HIR optimizer had a forwarding pass
(`ho_mem_fwd`), but within a block only, keyed by the address *value*
-- two `gaddr` of one item are two addresses -- and cleared at every
call.  csv2fw's per-byte path is 190 blocks with a call in most of them.

`ho_mem_avail` (src/hir/hir_opt.h, the copy's divergence list; an
upstream candidate) keys what is known by LOCATION -- a frame slot or a
global symbol, a byte offset, a width -- and carries it across the CFG
as available expressions are carried: OUT starts at top, a block's
entry knows what every predecessor's exit knows with the SAME SSA value
(no phi is made), reverse postorder to a fixpoint, then a load of a
known location becomes a COPY.  A store kills what it may overlap by
`ho_may_alias`'s rules -- two symbols never share storage, which holds
because a redefining item has no label of its own (layout.h) -- and a
store at a computed position inside an item (`raw-text(raw-len:1)`)
forgets that item alone, where before it was unknown and forgot
everything.  A call kills everything except what a known callee leaves
alone: `cob_refmod_len_chk`, `cob_refmod_len`, `memcmp` write nothing;
`memcpy`, `memmove`, `cob_fill` write their destination, of a constant
count when it is one.  Text nodes and the rest kill all.

Measured on csv2fw, both sides built against today's libcob (a pass
switched off by `S32_HIR_OPT_MASK=63487`, the knob added for the
purpose), alternating under one DBT:

    the per-byte island's loads     68  ->  44
    instructions             3,299,762,755  ->  3,244,824,786   -1.7%
    time                               202 ms  ->  196 ms        -3%

A load costs the DBT more than its instruction count says (an address
translation and a bounds check per access), so the time moves more
than the count.

What the pass does not reach, in order of value:

- **Joins.** After an EVALUATE arm that stores `f-owned`, the join sees
  two values for it and drops the location; the next arm reloads.  A phi
  at the join would keep it (load PRE).  Most of the 44 are these.
- **Partial forwarding.** `state` is stored as two bytes and read a
  byte at a time (the DISPLAY decode); the store's value is known but
  the byte loads do not match its width.  Extracting the byte from the
  known halfword is a shift and a mask, which needs an instruction
  inserted mid-block, which the optimizer's block ranges do not allow.
- **Text nodes.** After a `.Ltext` call the natives it may touch are
  reloaded by design.

### A measurement trap, recorded

The hook tag is the checksum of `kern.h`.  Adding the sort kernel
changed it, and every `.s32x` built before -- the four csv2fw builds
kept for A/B -- lost its COBOL hooks under the rebuilt DBT: 176 ms
became 216 with nothing else different.  An A/B across a kern.h change
must rebuild both sides; `slow32-dbt -s` shows "Hooks: 4" (the
builtins alone) when a binary's tag is stale.  Majesty's programs were
rebuilt (`s32x/build.sh`); the fleet's images are built with their
DBT, so they stay consistent.

## 2026-10-08: the fill held back until a path needs it

What a crossing costs, measured at last (a C program under the DBT, ten
million iterations, the bare loop subtracted): a native `memcpy` or
`memset` of 8 bytes 3.2 ns, a guest call 2.5, an inline 8-byte copy 0.7
-- so the crossing is cheap, barely a call.  The bytes are not: a
`memset` of 4 KB is 41 ns, and csv2fw does 1,050,000 of them, two per
field (`MOVE SPACES TO FLD-TEXT(k)`, then `MOVE OWN-TEXT(1:n) TO
FLD-TEXT(k)`, whose padding fills the rest again).  Some 43 ms of its
196.

The first fill is dead wherever the second move runs, and the second
move is conditional (`IF CUR-LEN > 0`), so the fill is dead on some
paths and needed on others: partial dead-store elimination, by sinking.
The lowering now holds such a fill back (`lower.h`, `pf_*`): a MOVE of
a figurative constant to one whole item, its subscripts items or
literals, is not emitted where it stands.  Statement by statement after
it: a MOVE into the whole of the same item that puts what the fill
would have -- a figurative sender, or any sender when the fill is
spaces (an alphanumeric MOVE pads with spaces), or a sender at least as
long -- makes the fill dead; a statement that may read or write the
item's record or write one of its subscripts, a loop, a text node, a
GO TO, an operand this reading does not analyse, or the end of an IF
arm or of the run, has the fill emitted before it.  An IF takes the held
fill down both arms; each arm settles it.  Sound by construction: no
path observes the fill's absence.  `S32_HIR_HOLDFILL=0` emits every
fill where it is written; the trace counts "fills held back: N dead
under a covering move, M emitted where a path needed them" per island.

    csv2fw   instructions  3,244,824,786 -> 2,304,117,102   -29%
             DBT, alternating      196 ms  ->  171 ms       -13%
             output byte-identical

(The instruction count falls further than the time: under the reference
interpreter the 4 KB fill is a thousand guest instructions; under the
DBT it is one native call.)

`tests/free/holdfill` pins the rule's edges inside an island: a covered
fill, one arm covering, a DISPLAY in between (which must show the fill),
a subscript changed in between (the fill lands on the old element), a
part moved into (the fill stands), nested arms with one path uncovered,
a loop in between, and ZEROS with a shorter sender (the move's own
padding decides).  The HIR build, the text build and the held-fill-off
build print the same bytes, which are the expected file.

## 2026-10-08: line sequential records, the short way

The reports' profile after the sort went native (gl034, guest-only
instructions): `cob_read_rest` 167 a record, `cob_write_rest` 147 plus
`fwrite` 54 and `fputc` 20 -- the line sequential paths, which had no
short entry of the kind the fixed-length records got on 2026-10-02.
Most of each is the long function's frame (twenty saved registers) and
the tests on the way to the case that always happens.

Two short paths in `cob_read_n` and `cob_write_n`, each taken when the
long path has set the flag on the file's first record (`fast_r`,
`fast_w` = 2 -- the fixed-record paths are 1):

- READ: a plain line sequential file open for input, a line that ends
  inside the block buffer and fits the record.  `memchr` for the
  newline, the bytes copied, a CR before the LF dropped, the rest
  spaces, 00.  A line cut by the buffer's end, a long line (04, or 06
  under rule 15) and the end of the file go the long way, which sets
  the flag again at the next line that fits.
- WRITE: a plain WRITE (no ADVANCING phrase, no BEFORE beside an AFTER)
  to a line sequential file, the print-file rule for AFTER 1: a newline
  unless the file is at its top, the record without its trailing
  spaces, the cursor left on the ink -- the long path's own steps, for
  this case alone.

The trailing spaces are trimmed sixteen bytes at a time by word loads
at any address, which the machine permits (docs/SPEC.md).  Not
`memcpy(&w, p, 4)`: libcob is compiled with `-fno-builtin`, under which
clang keeps that a call -- 264 of them in libcob.s today, each a
crossing -- where a plain `*(const unsigned *)p` is one `ldw` from both
compilers (stage08's does not parse an `aligned(1)` attribute in a
typedef, which the selfhost-libcob gate said).  The first attempt did
the memcpy and made the reports slower; the second is faster than the
byte loop it replaced.  The gates also caught the WRITE flag being set
by a national file's inner call (its UTF-8 copy, `varying` 3), which
would have skipped the conversion from the second record on:
2002/natfiles and 2002/lsrule15, byte for byte.

Batch instruction counts, before and after (the per-run counter):

    gl037 -7.6%   gl043 -7.1%   gl033 -6.9%   gl042 -6.9%
    gl041 -3.8%   gl035 -3.7%   gl034 -3.3%   gl038 -2.4%
    the batch -2.1% (csv2fw, which has no line sequential file, 0)

What remains per record in the reports, in order: `sort_key_build` 230
(a twenty-one-register frame for two alphanumeric keys: the wide and
floating-point key paths inlined into it), the indexed READ (gl035:
`cob_read_key_1` 145, `idx_find` 142, `bt_read` 73, `bt_pin` 61 a call),
`cob_return` 55, `cob_release` 52, `file_result` 32.

## 2026-10-08, evening: measured and declined

Three things the profile pointed at, each measured to a number and
left alone, so the next pass does not measure them again:

- **The hook crossings.**  csv2fw makes 3.5 million crossings to native
  routines a run (memset, memcpy, the numeric hooks).  Measured at 3 ns
  each (ten million `memcpy` calls of 8 bytes under the DBT, the bare
  loop subtracted; a guest call is 2.5 ns, an inline 8-byte copy 0.7).
  That is 10 ms of 160, and halving it with whole-operation hooks
  (stage 2's numeric MOVE as one crossing) would buy 1.6%.  Stage 2 is
  not worth building for this workload.
- **The per-record runtime of the reports.**  After the sort went native
  and the line sequential paths got their short entries, what is left
  per record is `sort_key_build` (230, now 161: the wide and float key
  forms moved out of its frame, which still saves twenty registers for
  the loops' live values), the indexed READ chain (`cob_read_key_1`
  145, `idx_find` 142, `bt_read` 73, `bt_pin` 61), `cob_return` 55,
  `cob_release` 52, `file_result` 32.  Each is 1-2% of its program;
  the split of `sort_key_build` moved gl034 by 0.1%.
- **Process startup.**  The batch is 28 emulator runs; an empty program
  takes 2.82 ms wall.  A C program that links libm takes 2.28 ms to
  spawn on this machine, so the DBT's own startup -- the 256 MB and 32
  MB mappings, the block tables, loading a 511 KB program, 580 page
  faults against the C program's 230 -- is about half a millisecond a
  run, 3% of the batch at the very most.  Not worth a DBT change and
  the three differentials it costs.  The spawn floor itself (dyld and
  the kernel) is 13% of the batch and is the operating system's.
- **Translation.**  The DBT's `-p` had put translation at a quarter of
  each report's run.  A host time profile of the DBT (Instruments'
  command-line tracer, inlining off so the translator's functions show,
  four reports pooled) finds translated code at 44%, the sort's key
  compare at 12%, record copies and the I/O syscalls, and the
  translator's own functions in single samples.  The `-p` attribution
  was not to be trusted; translation is not a lever here.

Where the day ended, the batch timed per run:

    this morning     616 ms    (the lock layer in front of the short entries)
    cc74a549         565       the entries restored
    049f67bf         498       the SORT's run as a native hook
    4dd9ba2c, 8acdfca5 476     the fill held back; line sequential short paths

csv2fw 250 -> 161 ms inside the batch; gl034 46 -> 38; gl038 51 -> 35.

What is left, by estimated size on csv2fw: a base register for the
data page (a fifth of the island's instructions are `lui` of the same
page; a day's work in the HIR copy, 5-10%); the second 4 KB fill per
field, the MOVE's own padding, 13% of the run and the program's by
design; phis at the joins for the loads (3%); the byte out of a known
halfword for the DISPLAY `state` (3%); the one-byte WRITE's fast path
in line in the island and the status test after each READ/WRITE call
(8 instructions where one would do; 2-3% each).

## 2026-10-08, afternoon: kstring and kidx, the two kernels behind GnuCOBOL

The nine kernels against GnuCOBOL at lunch: faster on seven, behind on
kstring (1.6x) and kidx (1.17x, and slower than on 2026-10-02).  The
user: faster on everything, if possible.  Profiles first.

**kidx** (guest-only instructions): a quarter in wide decimal
arithmetic for `COMPUTE K = FUNCTION MOD(I * 7919, N) + 1` -- the
register trees took MOD only by a literal, and a function argument goes
wide -- and the B-tree pinning a page 12.7 times a record, descending
four times, with 1.4 file requests a record.  The index cache was the
answer to the second: `S32_INDEX_CACHE` swept, 0.38 s at the default
(a cap of 256 pages, 1 MB), 0.25 at 1024, 0.24 at 4096; the cap is now
4096, the heap's sixteenth still the rule.  For the first, MOD and REM
by an item in the checked 64-bit path (arith_reg.h): the divisor is a
leaf with a test for zero, which sends the statement to the stack's
code, whose answer for a zero divisor stands; MOD takes the divisor's
sign by a 64-bit add when the remainder's sign differs; and COMPUTE now
tries the checked path when a tree was refused only for want of it
(`g_hn_wants_chk`), not only when an intermediate could pass 18 digits.
The islands refuse an item divisor (they have no slow path).
`tests/free/moditem` pins signs, zero, 18-digit dividends, inside and
outside a loop, against `-fno-hot-arith` and `-fno-hir`;
`gen-checked.py` has two shapes with item divisors, 60 programs the
same as HEAD's compiler.

    kidx   4,168,254,502 -> 2,311,355,641 instructions   393 -> 186 ms

**kstring** was all libcob's string runtime: INSPECT's general pass 348
a call, CONVERTING 510, UNSTRING's receivers through the general
`cob_move` (352 for the numeric one, 158 for the others), STRING's
source step 87, and some 330 instructions of INSPECT setup per
statement across four calls.  Four changes:

- the byte sweeps as hooks (docs/dbt-hooks.md step 5): `cob_bytes_xlat`
  (CONVERTING's table over the item) and `cob_bytes_sweep` (the
  one-byte TALLYING/REPLACING phrases by a 256-entry table), kern.h
  kernels compiled into libcob and the DBT, one crossing an INSPECT;
- the plain INSPECT forms as one call: an alphanumeric item or part, no
  BEFORE/AFTER, not BACKWARD -- CONVERTING literal TO literal
  (`cob_inspect_convert_plain`, the table kept across calls with the
  same literals), one TALLYING phrase FOR CHARACTERS or FOR ALL of one
  byte (`cob_inspect_tally_plain`, its count into phrase 0 for the
  compiler's ADD);
- UNSTRING's common receiver first: one delimiter of one byte, not ALL,
  an alphanumeric or an unsigned DISPLAY integer receiver, no DELIMITER
  IN or COUNT IN -- `memchr`, the bytes and spaces, out of the general
  routine's twenty-register frame;
- `cob_move` of digits-only text into an unsigned DISPLAY integer: the
  rightmost digits that fit, zeros before them (what the reading and
  `cob_put_num` store), 352 instructions to a copy; and `cob_str_src`'s
  dynamic-length growth out of its frame (16 saved registers to 9).

    kstring   2,561,126,373 -> 1,927,626,552 instructions   ~235 -> 168 ms
    (GnuCOBOL 3.2: 140)

The kernel differential covers the two sweeps (40 random runs each,
hooks called, declined 0).  Then `cob_str_src_size`: a STRING source
DELIMITED BY SIZE into a plain receiver, which the compiler emits for
that case (string_stmt.h), 73 -> 55 instructions a source -- and no
leaner from here: the LLVM backend splits the runtime's `cs` state
into separate globals, three instructions a field, and saves seven
registers around a `memcpy`.  kstring 2,561 -> 1,910 M instructions,
~235 -> 162 ms; GnuCOBOL 3.2 is 140.  Left: the UNSTRING entry (121
a receiver, the same frame cost), the program's own code 20%, and the
backend's two items -- shrink-wrapping and a struct addressed by one
base -- which would take a fifth off every libcob entry at once.
