# Record framing

A COBOL record is a payload. How that payload sits on a byte-stream
OS is a separate fact, recorded on the FD. Mixing the layers is how
a file "works" in one program and is garbage in tapemgr.

cobc370's Sequential I-O notes (`COBOL74-ROADMAP.md`, "Dataset
formats") split three layers on MVS: DCB merge, the compiler's DCB
table, QSAM. On SLOW-32 there is no DCB and no QSAM. The compiler
owns framing.

## The layers

| layer | when it appears | what it is |
|---|---|---|
| **Payload** | always | the `01` the program named |
| **RDW** | sequential, recording mode V (or inferred from `RECORD CONTAINS m TO n` / unequal `01`s) | 4 bytes, length-includes-self, two zero control bytes, big-endian |
| **Newline** | `ORGANIZATION IS LINE SEQUENTIAL` | a terminator, not a length |
| **Delete flag** | indexed/relative **if** the file is dBase-shaped | one byte, `' '` / `'*'`, *before* the payload, *not* part of the COBOL record |
| **BDW / blocking** | never on a host disk file | tapemgr's problem when a tape is being made |

Fixed sequential is payload only. Relative fixed is
`slot = RRN × LRECL`, holes allowed, no RDW.

## RDW, exactly

`~/majesty/src/cpp_standalone/tapemgr.cpp`, `VariableBinaryWriter`:

- 2 bytes length, big-endian, **including** the 4-byte RDW
- 2 control bytes, zero
- then the payload

`VariableRecordProcessor` on the read side skips those 4 bytes and
hands the payload to the caller.

cobc370 plants the same four bytes in a cell immediately before the
record area, which is where QSAM move-mode wants them. On SLOW-32
the runtime reads and writes the same on-disk prefix. There is no
BDW in the file. A "VB" dataset on Unix is a stream of RDW+payload.

This is the format sequential V must speak if anything round-trips
through tapemgr or cobc370's V files.

What a program sees in the record area *beyond* a short record is
implementor-defined. cobc370 moves the record in and leaves the
rest as it was; IBM locate-mode shows the next record's RDW. The
standard (85, Sequential I-O) says only that the record is made
available. Follow cobc370: move, do not promise the tail.

## LINE SEQUENTIAL is not V-mode

Majesty's batch path is line sequential. `csv2fw` writes
newline-terminated records; text fields go out at actual length,
not padded to the picture. `data-formats.md` is explicit.
`gl022`/`gl023`/`gl030` `SELECT` those files
`ORGANIZATION IS LINE SEQUENTIAL`.

A file of newline-terminated accounts is not a V dataset that lost
its RDWs. `ASSIGN` plus `ORGANIZATION` pick the framing. The
compiler implements both. LINE SEQUENTIAL is mandatory to retire
GnuCOBOL. RDW is mandatory to meet tapemgr.

Line sequential on write: payload, then `\n`. On read: up to `\n`
or the maximum record, strip a trailing `\r` if present so CRLF
hosts do not pollute the last byte. A record longer than the FD
is an error (file status), not a silent split.

## Sequential F

`ORGANIZATION IS SEQUENTIAL` (not LINE), recording mode F:
payload of `LRECL` bytes, no delimiter. `w001` and `glacpost`
use this. The length is the record description.

## Delete flags

COBOL `DELETE` on INDEXED is an erase (VSAM-shaped: the record is
gone, a later `READ` by that key is invalid key). dBase `DELETE`
is a mark (`'*'` at byte 0) until `PACK`.

Those are different verbs. Do not store them as the same byte with
two names unless the FD has opted into a dBase-shaped data file.
Default indexed files have no delete byte and a `DELETE` removes
the key and the slot.

When a delete byte *is* present (compatibility writer, later): it
sits before the payload, is not in the COBOL record, and is not an
RDW. A file does not carry both an RDW and a dBase delete byte in
v1; that combination is "indexed variable," which dBase cannot
represent anyway. See [indexed.md](indexed.md).

## Inference, IBM-shaped, adapted

cobc370 infers V from `RECORDING MODE V`, `RECORD CONTAINS m TO n`,
or 01s of different lengths under one FD. Do the same for
`ORGANIZATION IS SEQUENTIAL`. Do **not** infer V from line
sequential: the organization already named the framing.

`RECORDING MODE U` and `S` are refused, as in cobc370.

## What tapemgr still owns

Blocking, BDW, spanned (`VS`/`VBS`), EBCDIC translation, tape labels.
A SLOW-32 COBOL program sees unblocked files. If a tape must be
written, tapemgr reads our disk representation (F, or V-with-RDW)
and wraps it. That is why the RDW on disk has to be IBM's and not
a private length word.
