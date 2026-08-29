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
ours. Document it when it exists. It does not have to be `.NDX`.

`DELETE` removes the key and frees the slot. A subsequent `READ` by
that key is invalid key. No `PACK` step.

## Relative

`ORGANIZATION IS RELATIVE`, `RELATIVE KEY` an integer item. Slot =
`RRN × LRECL`. Empty slots are invalid key. No RDW. Majesty uses
this for journal entries in the `cr`/`ld`/`ex` programs; it is
after v1 unless a v1 program needs it. The slot machinery is the
same as indexed data pages without the btree.

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
