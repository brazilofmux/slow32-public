# dBase III Clone for SLOW-32

A dBase III Plus compatible database management system, built in C
against the MMIO runtime. Target: run real 1986 dBase III programs.

## Current Status

**44/44 tests passing.** Core engine is solid. See [STATUS.md](STATUS.md)
for the complete feature inventory.

What works today:
- DBF file format (CREATE, USE, read/write records)
- 75+ commands (navigation, editing, display, file ops, bulk ops)
- 39 built-in functions (string, numeric, date, status)
- 13 SET options
- Expression evaluator with lexer-based recursive descent
- 10 work areas with cross-area field references
- Memory variables with PRIVATE/PUBLIC scoping
- Programming language (PRG files, IF/ELSE, DO WHILE, DO CASE, FOR/NEXT)
- Procedures and user-defined FUNCTION with parameters
- Screen I/O (@SAY, @GET, READ, CLEAR, box drawing)
- Index files (INDEX ON, SEEK, FIND, multi-index, SET ORDER)
- Macro substitution (&varname)
- SET FILTER, SET PROCEDURE, scope clauses

## What's Next

See [ROADMAP.md](ROADMAP.md) for the prioritized work plan.
See [TEACHERS-PET-ANALYSIS.md](TEACHERS-PET-ANALYSIS.md) for the
feature gap analysis against the akron/ legacy application.

Short version: ~8 small gaps block Teacher's Pet from running.
Phase 1 (no-op SETs, CLOSE DATABASES, RELEASE ALL LIKE) is a
single session of work. Phase 2 (SET DEVICE, RANGE, ROW/COL)
is another 1-2 sessions.

## Architecture

dBase III is a database management system with:
- A **dot prompt** command interpreter
- **DBF file format** for tabular data (fixed-length ASCII records)
- **Expression evaluator** for queries and computed fields
- **Full-screen terminal UI** for data entry (`@...SAY...GET`)
- A **programming language** (`.PRG` files with control flow)
- **Index files** (`.NDX`) for fast lookups
- **Report/label generators** (not yet implemented)

## Data Types

| Type | Code | Storage | Example |
|------|------|---------|---------|
| Character | C | ASCII, space-padded | `"SMITH     "` |
| Numeric | N | ASCII decimal | `"  1234.56"` |
| Date | D | 8-byte YYYYMMDD | `"19850315"` |
| Logical | L | 1-byte T/F/Y/N | `"T"` |
| Memo | M | 10-byte block number | (deferred) |

All field data is stored as printable ASCII text in fixed-width fields.

## Implementation History

| Stage | Focus | Status |
|-------|-------|--------|
| 1 | [DBF Engine](STAGE1-dbf-engine.md) — file format + REPL | Complete |
| 2 | [Navigation & Expressions](STAGE2-navigation-expressions.md) — movement + evaluator | Complete |
| 3 | [Variables & Commands](STAGE3-variables-commands.md) — vars, SETs, work areas, bulk ops | Complete |
| 4A | Programming — PRG files, control flow, procedures, functions | Complete |
| 4B | Screen I/O — @SAY/@GET/READ, CLEAR, box drawing, colors | Complete |
| 4C | Indexing — INDEX ON, SEEK, FIND, multi-index, SET ORDER | Complete |
| 5+ | [Roadmap](ROADMAP.md) — Teacher's Pet compatibility | In progress |

The original stage 4-6-7 plan was reorganized during implementation.
Programming, screen I/O, and indexing were delivered as 4A/4B/4C
sub-stages. The original stage docs (STAGE4-6, OPTIMIZATION) are
preserved as historical reference but are superseded by the new
ROADMAP.md and STATUS.md.

## Build & Test

```bash
cd dbase && bash build.sh
bash tests/run-tests.sh
```

Run interactively:
```bash
echo 'QUIT' | ../tools/emulator/slow32-fast dbase.s32x
```

Run a program:
```bash
(echo 'DO myprogram'; echo 'QUIT') | ../tools/emulator/slow32-fast dbase.s32x
```

## DBF File Format (Quick Reference)

```
Header (32 bytes):
  [0]    version byte (0x03 = dBase III)
  [1-3]  date of last update (YY MM DD)
  [4-7]  number of records (little-endian uint32)
  [8-9]  header size in bytes (little-endian uint16)
  [10-11] record size in bytes (little-endian uint16)
  [12-31] reserved

Field descriptors (32 bytes each):
  [0-10]  field name (NUL-padded)
  [11]    field type (C/N/D/L/M)
  [12-15] reserved
  [16]    field length
  [17]    decimal count
  [18-31] reserved

Terminator: 0x0D byte

Records:
  [0]     deletion flag (' ' = active, '*' = deleted)
  [1..]   field data (ASCII, space-padded, fixed width)

EOF marker: 0x1A (optional)
```
