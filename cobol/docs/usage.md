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
| COMP-1 | RM/COBOL's: a binary integer *with* a PICTURE (BP-E3) | as COMP |
| COMP-2, FLOAT-SHORT, FLOAT-LONG | refused: "floating-point USAGE is not implemented" | |
| COMP-4, COMP-6, COMP-X, COMP-N | not recognized | |
| BINARY-CHAR/SHORT/LONG/DOUBLE, SIGNED-INT etc. | native binary | 1/2/4/8 |

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
- **COMP-5:** as COMP-X, but signed and in the machine's own byte order.
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
