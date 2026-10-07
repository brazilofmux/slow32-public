# The computational usages: COMP, COMP-n, byte order, floating point

**Ruled 2026-09-30 by the user, as proposed below:**
- COMP, BINARY and COMP-4 are big-endian, with
  `-fbinary-byteorder=native` as the escape hatch;
- COMP-1 is told apart by its PICTURE (with one, RM's binary; without,
  an IEEE float), `-fcomp1=` forcing either;
- COMP-1 and COMP-2 DISPLAY in MF's and IBM's `-.9(8)E-99` and
  `-.9(18)E-99` forms.

A proposal, 2026-09-30. It covers where s32-cobc stands today, what Micro
Focus does (the lead chosen for floating point), what the corpora use,
and the decisions to make. The standard leaves most of this to the
implementor: BINARY's size and byte order, and every COMP-n. So this is
a dialect question, decided for preservation.

## Today

| usage | s32-cobc now | size |
|---|---|---|
| COMP, COMPUTATIONAL, BINARY | two's complement, the picture's digits the limit | 2/4/8 bytes by digits (IBM's rule) |
| COMP-5 | the same, the binary field's capacity the limit (BP-E3) | 1/2/4/8 |
| COMP-3, PACKED-DECIMAL | packed decimal, sign nibble C/D/F | digits/2 + 1 |
| COMP-1 | with a PICTURE, RM/COBOL's binary integer (BP-E3); without one, MF's IEEE single (step 3); `-fcomp1=binary\|float` forces either | as COMP; 4 |
| COMP-2, FLOAT-SHORT, FLOAT-LONG | IEEE double (FLOAT-SHORT single), no PICTURE (step 3); FLOAT-* under -std=2002 | 8 (4) |
| FLOAT-BINARY-32, FLOAT-BINARY-64 | COBOL 2014 (-std=2014): IEEE binary32 and binary64, the hardware's, as FLOAT-SHORT and FLOAT-LONG -- the same arithmetic in double, MF's DISPLAY form; the byte order the item's endianness phrase or the OPTIONS FLOAT-BINARY default, else the machine's (HIGH-ORDER-RIGHT) | 4, 8 |
| FLOAT-BINARY-128 | 2014: IEEE binary128 in software (libcob/ieee.h); the value read as a decimal of 36 significant digits, written back correctly rounded to nearest-even | 16 |
| FLOAT-DECIMAL-16, FLOAT-DECIMAL-34 | 2014: IEEE decimal64 and decimal128, BID (BINARY-ENCODING, the default) or DPD (DECIMAL-ENCODING), the byte order as above; exact decimal values, 16 or 34 digits, exponents to 384 and 6144 | 8, 16 |
| COMP-4 | BINARY (step 2) | as COMP |
| COMP-X | MF's: unsigned, big-endian, the field's capacity the limit (step 2) | PIC X(n): n bytes; PIC 9(n): the fewest bytes holding n nines, 1-8 |
| COMP-6 | unsigned packed decimal, no sign nibble; a signed one is COMP-3 (MF's default COMP-6"2"; step 2) | (digits + 1) / 2 |
| COMP-N | not recognized | |
| BINARY-CHAR/SHORT/LONG/DOUBLE, SIGNED-INT etc. | native binary | 1/2/4/8 |

**The 2014 standard floating-point usages** (queue item 20, 2026-10-07).
A statement with a FLOAT-DECIMAL or FLOAT-BINARY-128 operand or receiver
computes on the wide decimal stack in a *floating* mode: every operand
is read as a decimal (a binary value of either hardware or software
format exactly, to 36 significant digits), the stack holds 38 digits and
sheds low digits for room instead of reporting a size error, a quotient
takes 36 digits, and the store rounds to the receiver's format
(nearest-even, IEEE's default; ROUNDED MODE is not applied to these
receivers). So 0.1 + 0.2 is 0.3 in decimal64, and a binary128
computation is decimal arithmetic rounded twice (into and out of the
format) rather than IEEE binary arithmetic -- NATIVE arithmetic leaves
the intermediates to the implementor (2023 8.8.1.3). A value past the
format is a size error (the receiver unchanged); below it, a subnormal
or zero. An infinity or NaN -- by a REDEFINES, or SET CONTENT OF ... TO
FLOAT-INFINITY / FLOAT-NOT-A-NUMBER[-SIGNALING] (2014, queue item 21) --
is not NUMERIC, not IN-ARITHMETIC-RANGE, reads as zero in arithmetic,
and DISPLAYs as Inf or NaN with its sign; the FLOAT-* class conditions
tell them apart, and FARTHEST-FROM-ZERO and NEAREST-TO-ZERO (the class
conditions and SET CONTENT OF) are each format's largest finite value
and smallest subnormal. IN-ARITHMETIC-RANGE is a ruling: NATIVE's
intermediates (38 decimal digits, doubles, floating decimals of 38
digits and any exponent) hold every finite value of every item, so the
phrase changes nothing and the condition is true of every finite
value. DISPLAY shows the
software formats as their significant digits, one before the point, and
a decimal exponent (`1.5E+00`, `3.333333333333333E-01`, `1E+3000`);
GnuCOBOL shows them as the stored coefficient and exponent. A
fractional power (2 ** 0.5) is still computed in double. The host test
tests/ieee_test.c checks every encoding against exact rational
arithmetic (tests/ieee_vectors.py).

**Byte order** (step 1, done 2026-09-30): COMP, COMPUTATIONAL, BINARY and
RM's COMP-1 are **big-endian**; `-fbinary-byteorder=native` keeps them
little-endian. COMP-5, the native usages (BINARY-CHAR and the rest,
POINTER, INDEX) and RETURN-CODE, a C int that libcob shares, are always
SLOW-32's own little-endian order. (Before step 1, every binary item was
little-endian.)

## What Micro Focus does (Visual COBOL 8.0 Language Reference)

- **COMP, BINARY, COMP-4:** two's complement, **big-endian** (high-order
  byte at the lowest address), whatever the machine. It is sized by
  digits in byte-storage mode by default; IBMCOMP gives word-storage.
- **COMP-X:** unsigned big-endian binary. Its PICTURE may be all X's
  (the length in bytes); the limit is the field's capacity.
- **COMP-5:** as COMP-X, but signed and in the machine's own byte order. Here
  `PIC X(n) COMP-5` takes 1 to 8 bytes: up to seven as an item of the
  digits they hold, and eight -- 2^64 - 1, twenty digits -- as BINARY-DOUBLE
  UNSIGNED, the same item, on the wide path, so -std=2002 (2002/comp5x8;
  ISSUES-120). COMP-X's eight-byte X picture is that item big-endian, also
  under -std=2002 (2002/mf-compx8; ISSUES-124).
- **COMP-1 and COMP-2:** IEEE 754 single and double, 4 and 8 bytes, no
  PICTURE; FLOAT-SHORT and FLOAT-LONG are the 2002 names. The storage
  "can differ from operating system to operating system", which in
  practice means the machine's byte order.
- **Directives:**
  - `COMP1"FLOAT"` is the default. `COMP1"BINARY"` (set by DIALECT"RM"
    and "ACU") makes COMP-1 an S9(4) COMP;
  - `COMP2"FLOAT"` is the default. `COMP2"DECIMAL"` makes COMP-2
    ACUCOBOL's unpacked decimal.
- **DISPLAY** shows COMP-1 as if it had the external floating-point
  PICTURE `-.9(8)E-99`, and COMP-2 as `-.9(18)E-99` (IBM's form: 1.5
  displays as ` .15000000E 01`).
- **Accuracy:** 7 digits for COMP-1, 16 for COMP-2.

IBM agrees on BINARY's big-endian order. Its COMP-1 and COMP-2 are
System/370 hexadecimal floating point by default. We will not support
that, beyond perhaps conversion routines later: MF's IEEE lead is the
one taken.

## What the corpora use

| corpus | usages (counts in the source) |
|---|---|
| majesty | COMP-5 132, PACKED-DECIMAL 76, COMP-3 64, BINARY-CHAR 4 |
| Open Systems (RM/COBOL) | COMP-3 738, COMP 522, COMP-1 98 (all with a PICTURE: RM's binary) |
| CCVS-85 | COMP 1,203, BINARY 4 |
| NIST SQL (embedded COBOL) | COMP 464, BINARY 29, COMP-3 4, COMP-1 1 (a float: dml035) |

None of them reads a data file written by another system's binary
fields. The Open Systems suite creates every file it reads, and majesty
keeps its money in packed decimal and its native values in COMP-5. So
no existing data pins the byte order down. The choice is about the
files still to come.

## The decisions

**1. COMP / BINARY / COMP-4 byte order: big-endian (proposed).**
- **Why:** MF, IBM and GnuCOBOL's default all store BINARY big-endian.
  So a record written by any of them, or by a mainframe, reads correctly
  only if we do the same, and preservation is mostly about reading such
  records.
- **Cost:** SLOW-32 has no byte-swap instruction, so every inline COMP
  load and store becomes a few more instructions: byte loads and shifts,
  or a load and a swap sequence.
- **Who pays:** majesty keeps its hot values in COMP-5, which stays
  native, and pays nothing. CCVS, the Open Systems suite and NIST pay a
  little.
- **An escape hatch:** `-fbinary-byteorder=native` would keep today's
  code for programs that write only their own files.
- **What it breaks:** nothing in the tree reads another system's
  binary, and every gate's outputs are printed text, which byte order
  does not reach.

**2. COMP size stays IBM's 2/4/8.**
- MF's byte-storage default packs PIC 9(5) COMP into 3 bytes, while
  IBM, GnuCOBOL's default and the SLOW-32 C ABI use 4.
- Mainframe records are the more common interchange, so keep 2/4/8.
- MF's byte-storage sizing could be added later as a flag if an MF data
  file needs it.

**3. COMP-1 and COMP-2 are IEEE floats, as MF has them; COMP-1 with a
PICTURE stays RM's binary.**
- A float COMP-1 takes no PICTURE, so the presence of one tells the
  dialect apart without a directive. This keeps the Open Systems suite's
  98 COMP-1 items as they are.
- A `-fcomp1=binary|float` flag would force either reading.
- COMP-2 with a PICTURE (ACU's unpacked decimal) is refused, as not
  implemented.
- FLOAT-SHORT and FLOAT-LONG are the same under -std=2002.
- Floats are stored in the machine's byte order, little-endian.

**4. Floating-point semantics (MF and IBM agree):**
- **Arithmetic:** a statement with a float operand or receiver computes
  in double precision (SLOW-32 has hardware double arithmetic).
- **MOVE** to and from a float converts. Numeric to float rounds to the
  nearest representable value; float to numeric truncates to the
  receiver's scale, with a size error where the ON SIZE phrase asks.
- **DISPLAY** of COMP-1 and COMP-2 uses MF's `-.9(8)E-99` and
  `-.9(18)E-99` forms.
- **Comparisons** convert to double.
- **Host variables:** the ESQL runtime binds a float as SQLite's REAL.

**5. The small ones:**
- COMP-4 is BINARY;
- COMP-X is MF's unsigned big-endian binary, PIC 9(n) or X(n);
- COMP-6 is MF's and RM's unsigned packed decimal, no sign nibble;
- COMP-N is not planned unless code uses it.

Each is a class E behavior point, as COMP-3 and COMP-5 are now.

## The order of work

**Step 1 as built:**
- A descriptor carries the order in a second flags byte (`flags2`,
  `COB_F2_BIGEND`; the first byte is full). kern.h reads and writes by it,
  so the DBT hooks do too, and so do libcob's 31-digit paths.
- The compiler's inline loads and stores of a hot COMP item (a word or
  less, integer) work without a byte-swap instruction, which SLOW-32
  lacks, and with r2 as the only scratch register:
  - a halfword load is two byte loads;
  - a word load is a word load and nine register operations;
  - a store is the bytes one at a time.
- VALUE clauses are laid down big-endian at compile time.
- Under `-fbinary-byteorder=native` the generated code is byte-identical
  to the code before step 1.
- `tests/free/binorder.cbl` writes a record and prints its bytes, with
  GnuCOBOL as the oracle.

**Step 2 as built:**
- **COMP-4:** BINARY under another name.
- **COMP-X:** COMP-5's rules (no truncation to the picture) with a
  variant mark (`Sym.uvar`):
  - always big-endian, whatever `-fbinary-byteorder` says, as MF has it;
  - never signed;
  - PIC X(n) up to eight bytes: up to seven taken as the 9s of 256^n - 1,
    eight as BINARY-DOUBLE UNSIGNED stored big-endian (-std=2002);
  - it shows, as COMP-5 does, at its capacity's width (three bytes, eight
    digits);
  - GnuCOBOL truncates COMP-X to the picture and shows the picture's
    digits. That is a documented divergence (docs/oracles.md): the usage
    is MF's, so MF's reference decides.
- **COMP-6 unsigned:** packed decimal with the descriptor's
  `COB_F2_NOSIGN` flag, every nibble a digit. This is in kern.h (so in
  the hooks), in the 31-digit paths and in the class test. A signed
  COMP-6 is COMP-3, as MF's default and GnuCOBOL have it.
- **CALL signatures** (.s32fn) carry the variant in the usage's second
  byte, so COMP-X does not conform to COMP-5.
- **Micro Focus's two further COMP-X rules**, found when its reference
  was walked against the tests:
  - a negative value MOVEd into a COMP-X item is stored in two's
    complement, "as if the item had been signed" (the descriptor flag
    `COB_F2_TWOSC`, on a MOVE's store only; arithmetic keeps the
    standard's magnitude);
  - with ON SIZE ERROR, a 9(n) item's n digits decide the size error,
    while without the phrase it still stores to its capacity
    (`COB_F2_SIZEDIG`).
  - Both are in kern.h, so the DBT hooks honor them.
  - `tests/free/compxmf` checks both.
- **COMP-5 with a PICTURE of X's** (the same MF page): n bytes, unsigned,
  held to capacity, in the machine's order. It was refused before, with
  a message citing a standard rule that does not cover COMP-5.
  `tests/free/comp5x`; GnuCOBOL stores it big-endian, as if it were
  COMP-X (docs/oracles.md).
- **Tests:** `tests/free/compn.cbl` checks sizes, bytes and arithmetic,
  and the `bad/compx-*` tests check the refusals.

**Step 3 as built:**
- **Storage:** IEEE single or double in the machine's order; descriptor
  usage `COB_U_FLOAT`.
- **Arithmetic** runs on the wide stack, whose `cob_wnum` can hold a
  double.
  - A statement with a float operand or receiver is marked (`g_fstmt`),
    and every operand goes on the stack as a double (`cob_fpush`). The
    whole statement is computed in double: `2 / 3 * f` is not `2 / 3`
    in decimal first.
  - A store into a decimal receiver truncates, or rounds under ROUNDED,
    to its scale, then stores as any wide value, size error included.
- **MOVE and comparison** convert through double. SORT keys order by the
  double's bits.
- **DISPLAY** uses MF's form, `-.9(8)E-99` and `-.9(18)E-99`: the
  mantissa's digits from `%.*e`, so the COMP-2 digits past the double's
  precision are those of the exact binary value.
- **The narrow paths** (subscripts, intrinsic arguments) take a float's
  value at nine decimals.
- **ESQL** binds a float host variable as REAL and fetches the column's
  double, not its text. NIST dml035 passes.
- **`**` with a fractional or negative exponent**, refused before (a
  stop at run time), is now computed in double on both stacks. A zero
  base to such a power, or a negative base to a fraction, is a size
  error.
- **Micro Focus's float rules, walked page by page** (its Language
  Reference: the float formats, ROUNDED, MOVE, DISPLAY, subscripting,
  reference modification, the class condition, DIVIDE, PERFORM, SEARCH,
  CALL):
  - an arithmetic statement's floating-point result is always rounded
    into a decimal receiver, ROUNDED being documentary; a MOVE still
    truncates, as a numeric MOVE does;
  - a float subscript, and a reference-modification position computed
    in floating point, are rounded to the nearest integer. The position
    had failed to compile, with a misleading message about 18 digits;
  - an 88 on a float works;
  - refused, as MF refuses them:
    - a class condition on a float;
    - a float in DIVIDE ... REMAINDER;
    - PERFORM TIMES a float;
    - CALL BY VALUE a float (a COMP-1 had passed as a word);
    - a SEARCH ALL key that is a float;
    - a float UNSTRING receiver (refused already).
  - `tests/free/floatmf` (no oracle: GnuCOBOL refuses a float subscript)
    and `tests/bad/float-*` check these.
  - **Not taken:** MF rounds a float argument where an intrinsic expects
    an integer. Here it goes through the narrow stack at nine decimals
    and truncates. No program here passes one.
- **Not done:**
  - floating-point literals (`1.5E3`) and external floating-point
    PICTUREs (step 4, when code asks);
  - MOVE of a float to or from an alphanumeric item (bytes, as a group
    move);
  - CALL BY VALUE of a float.
- **Tests:**
  - `tests/free/comp12.cbl`: GnuCOBOL computes the same values, with
    the display form and the statement-in-double rule as documented
    divergences;
  - `tests/2002/floatsort.cbl`: GnuCOBOL agrees;
  - `bad/float-*`: the refusals.

1. **Byte order** of COMP/BINARY/COMP-4, with the flag. The runtime and
   the compiler's inline paths change together. The gates' printed
   outputs must not move, and a test writes a binary record and checks
   its bytes.
2. **COMP-4, COMP-X, COMP-6.**
3. **COMP-1/COMP-2/FLOAT-*:** storage, MOVE, DISPLAY, comparison, then
   arithmetic, each with a test. NIST dml035 and GnuCOBOL's IEEE floats
   serve as checks where GnuCOBOL agrees with MF.
4. **External floating-point PICTUREs** (`-.9(8)E-99` as a declared
   PICTURE, MF's and IBM's numeric-edited float), if code asks for them.
