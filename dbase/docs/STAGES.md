# dBase III Implementation Stages

A dBase III clone for SLOW-32, built in C against the MMIO runtime.

## Architecture

dBase III is a database management system with:
- A **dot prompt** command interpreter
- **DBF file format** for tabular data (fixed-length ASCII records)
- **Expression evaluator** for queries and computed fields
- **Full-screen terminal UI** for data entry (`@...SAY...GET`)
- A **programming language** (`.PRG` files with control flow)
- **B-tree indexes** (`.NDX` files) for fast lookups
- **Report/label generators**

## Data Types

| Type | Code | Storage | Example |
|------|------|---------|---------|
| Character | C | ASCII, space-padded | `"SMITH     "` |
| Numeric | N | ASCII decimal | `"  1234.56"` |
| Date | D | 8-byte YYYYMMDD | `"19850315"` |
| Logical | L | 1-byte T/F/Y/N | `"T"` |
| Memo | M | 10-byte block number | (deferred) |

All field data is stored as printable ASCII text in fixed-width fields.
This makes the format human-readable and simplifies I/O.

## Stages

| Stage | Name | Focus | Key Deliverables |
|-------|------|-------|-----------------|
| 1 | [DBF Engine](STAGE1-dbf-engine.md) | File format + minimal REPL | CREATE, USE, APPEND, REPLACE, LIST, QUIT |
| 2 | [Navigation & Expressions](STAGE2-navigation-expressions.md) | Record movement + expression evaluator | GO, SKIP, LOCATE, EOF(), string/numeric/date functions |
| 3 | [Variables & Commands](STAGE3-variables-commands.md) | Memory variables + bulk operations | STORE, SET, COPY TO, APPEND FROM, DELETE/PACK, work areas |
| 4 | [Terminal UI](STAGE4-terminal-ui.md) | Full-screen data entry | @...SAY...GET, READ, PICTURE, CLEAR, BROWSE |
| 5 | [Programming Language](STAGE5-programming.md) | PRG execution + control flow | DO, IF/ENDIF, DO WHILE/ENDDO, CASE, procedures |
| 6 | [Indexing & Reports](STAGE6-indexing-reports.md) | B-tree indexes + report generator | INDEX ON, SEEK, REPORT FORM, LABEL FORM |

Each stage produces a working, testable binary. Stage 1 is a usable
(if primitive) database tool; each subsequent stage adds capability.

## Build Pattern

Same as `sbasic/` and `basic/`:
```bash
cd dbase && bash build.sh
(cat commands.txt; echo "QUIT") | ../tools/emulator/slow32-fast dbase.s32x
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
