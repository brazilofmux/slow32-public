# Indexed and relative I-O

## The point

Majesty already has indexed files (`gl039` builds `tmp/descriptions.dat`;
`gl030` and `gl036` random-read it) and relative files (`crglentry`,
`ldglentry`, `exglentry`). v1's indexed claim is that path, not a
`.DBF` that `USE`s in dBase.

dBase III's DBF/NDX is a *constrained* indexed sequential. COBOL
INDEXED is a *general* keyed file. COBOL can describe records dBase
cannot store. That is expected. File-level compatibility is a
nice-to-have, implemented later as a **filter on the FD**, not as
the default writer.

## What to reuse from `dbase/`

Machinery, not the file format:

- page/slot allocation for fixed-length records
- a btree (or the existing `.NDX` code) keyed by a byte string
- an optional leading delete byte, with an honest story about when
  it appears (see [framing.md](framing.md))
- the lesson that indexes and data files can get out of sync, so
  `REBUILD`/`REINDEX` exists

Do not reuse:

- 11-character field names as a compiler limit
- types restricted to `C N D L M`
- `uint16` record size as a COBOL limit
- "every record the same length" as an indexed invariant
- delete-mark semantics as the meaning of COBOL `DELETE`

The dBase engine is a guest program. This compiler's indexed runtime
is `libcob`, linked into the COBOL program. Sharing *source* with
`dbase/src` is a later engineering choice; sharing *format* is not
required. If the btree implementation is copied, stamp a provenance
comment the way f77 stamps `hir_*.h`.

## COBOL indexed, default path

- `ORGANIZATION IS INDEXED`
- `RECORD KEY` is an elementary item in the record (gl039:
  `pic 9(10)` plus `pic x(80)`)
- `ACCESS MODE` sequential, random, or dynamic
- `FILE STATUS`
- `OPEN INPUT` / `OUTPUT` / `I-O`
- `READ` / `WRITE` / `REWRITE` / `DELETE` / `START`, with
  `INVALID KEY` / `AT END` / `END-READ` / `END-WRITE`
- fixed-length records for v1

On disk, v1 default: a data file of fixed slots (payload only, no
RDW, no delete byte) plus a key file. The key file's format is
ours. It does not have to be `.NDX`.

**As built (Stage 5, `libcob/libcob.c`):** the data file named by
`ASSIGN` holds `recsize`-byte slots in arrival order; beside it
`<name>.key` holds the key table:

    "S32KEY01" | u32 recsize | u32 keyoff | u32 keylen | u32 count
             | u32 nslots | u32 0 | u32 0 | count x (key bytes, u32 slot)

all little-endian, entries sorted by key (byte order, i.e. ASCII).
While open the table is an in-memory array: a random `READ` is a
binary search, `READ NEXT` walks it, `WRITE` of an ascending key
appends and an out-of-order key inserts in place, `DELETE` removes
the entry and leaves the slot unused, `CLOSE` rewrites the key file.
`OPEN` refuses (status 39) a key file whose recsize/keyoff/keylen do
not match the FD. A btree can replace the array without changing
program-visible behaviour; gl039's 3,113 descriptions take 0.13 s
under `slow32-fast` as it is. Status codes follow the text: 22
duplicate, 23 not found, 21 sequence error under ACCESS SEQUENTIAL,
10 at end, 35/39 on OPEN.

`DELETE` removes the key and frees the slot. A subsequent `READ` by
that key is invalid key. No `PACK` step.

### Alternate keys (2026-08-31)

`ALTERNATE RECORD KEY IS name [WITH DUPLICATES]`, any number. Each
key has a table of `(key bytes, u32 slot, u32 seq)` sorted by key then
`seq`, the arrival counter -- which is the order duplicates are
retrieved in. The key file is now `S32KEY02`: the prime table, then
`u32 nalt` and per alternate `u32 offset | len | dups | count` and its
entries; an `S32KEY01` file still loads and its alternate tables are
rebuilt from the records. A random `READ ... KEY IS k` or a `START
... KEY IS k` makes `k` the key of reference for `READ NEXT`; `START`
on an item that begins where a key begins compares that leading part.
`WRITE` and `REWRITE` refuse (22) a duplicate on a key without
DUPLICATES and report 02 on one with; `READ` reports 02 when the next
record under the key of reference has the same key value (the 1985
I-O status rule -- GnuCOBOL leaves the first record after a START or
random READ at 00, `free/altkey.oracle-expected`); `DELETE` and a
`REWRITE` that changes an alternate keep every table right. NIST IX:
28 of 29 programs, 405 of 406 tests, all matching GnuCOBOL's tally.

## Relative

`ORGANIZATION IS RELATIVE`, `RELATIVE KEY` an integer item outside the
record. Majesty uses it for journal entries in the `cr`/`ld`/`ex`
programs.

### As built (Stage 19, 2026-08-30)

Slot n is at `(n-1) × (4 + recsize)`: every slot carries the same
four-byte RDW as a mode-V record (big-endian length including the
four, then two zero bytes), all zero when the slot is empty, so a
relative file is a sequence of fixed-size V frames and a deleted
record is unambiguous -- the plan above said "no RDW", and the RDW
turned out to be exactly what makes empty and variable-length slots
representable (`RECORD CONTAINS 10 TO 98` is what glentry declares:
a 10-byte control record in slot 1, 98-byte data records after it).
The bytes past a short record are zero. Random access reads and writes
the slot the key names (22 occupied, 23 absent, 24 for key 0);
sequential access fills the next slot on WRITE and tells the key item
its number, READ NEXT skips empty slots and sets the key item, and
REWRITE/DELETE act on the last record read (43 when there was none);
START positions on the first occupied slot in the relation (last, for
`<` and `<=`) without touching the key item; OPEN EXTEND positions
after the last slot. All of it measured against GnuCOBOL 4 in
tests/free/relative -- every status and every line agree; the bytes
differ (GnuCOBOL keeps an 8-byte native length per slot,
docs/oracles.md), and no program outside COBOL reads these files.

## Where dBase cannot follow

These are legal COBOL and must not be silently rewritten to fit
dBase:

- `RECORD KEY` that is not a single dBase field (COMP-3, group,
  substring)
- `ALTERNATE RECORD KEY … WITH DUPLICATES`
- `OCCURS`, `OCCURS DEPENDING ON`, `REDEFINES` that change the
  apparent schema
- field names longer than 11 characters
- record length > 65535
- variable-length indexed records (RDW + key)
- more fields, or types, than dBase III stores

A program that writes those gets a file `libcob` can read and dBase
cannot. That is correct.

## Compatibility writer (later, not v1)

An FD-level restriction, named in a diagnostic if violated:

- every elementary item maps to C/N/D/L
- names ≤ 11 characters, record ≤ 65535
- no COMP-3, no ODO, no COBOL-shaped alternate keys
- leading delete byte, dBase header, `.NDX` keys from field
  expressions

Then, and only then, `USE` in `dbase/` is a test. Failure of that
test against a default indexed file is not a compiler bug.

## First acceptance test

`gl039`: read `data/descriptions_fixed_width.txt` (line sequential),
write indexed by `desc-id`, then `gl030` `READ` random by that key
while Report Writer prints. Matching `reports_cobol/journal-*.prn`
is the proof the index worked. Round-tripping the same bytes through
dBase is not.
