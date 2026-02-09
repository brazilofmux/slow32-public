# Stage 6: Indexing & Reports

## Goal

Implement `.NDX` index files for fast record lookup and the report/label
generators. This is the final stage, completing dBase III compatibility.

## Prerequisites

- Stage 5 complete (programming language)

## Index Files (.NDX)

### Commands

| Command | Syntax | Description |
|---------|--------|-------------|
| INDEX ON | `INDEX ON <expr> TO <file>` | Create index |
| SET INDEX TO | `SET INDEX TO <file> [, ...]` | Open index(es) |
| REINDEX | `REINDEX` | Rebuild all open indexes |
| FIND | `FIND <literal>` | Search index for key |
| SEEK | `SEEK <expr>` | Search index for expression |
| SET ORDER TO | `SET ORDER TO <n>` | Select controlling index |

### NDX File Format

B-tree with 512-byte pages:

```
Header page (512 bytes):
  [0-3]   root page number
  [4-7]   total pages
  [8-11]  reserved
  [12-13] key length
  [14-15] keys per page
  [16-17] key type (0=character, 1=numeric, 2=date)
  [18-21] key record size
  [22]    reserved
  [23]    unique flag
  [24-259] key expression (NUL-terminated)

Interior page (512 bytes):
  [0-3]   number of keys on page
  Key entries: [child_page (4)] [recno (4)] [key_value (key_length)]
  Trailing child pointer after last key

Leaf page (512 bytes):
  [0-3]   number of keys on page
  Key entries: [0x00000000 (4)] [recno (4)] [key_value (key_length)]
```

### B-Tree Operations

- **Search**: descend from root, compare keys, follow child pointers
- **Insert**: search to leaf, insert key, split if full, propagate up
- **Delete**: mark record deleted (don't remove from index; REINDEX rebuilds)
- **Sequential**: in-order traversal for indexed LIST/DISPLAY

### Integration

When an index is active (SET INDEX TO):
- GO TOP / GO BOTTOM follow index order
- SKIP follows index order
- APPEND automatically inserts into index
- REPLACE on indexed field updates index
- LOCATE/LIST FOR uses index when expression matches index key
- FIND/SEEK do B-tree lookup (O(log n) instead of O(n))

## Report Generator

### Commands

| Command | Syntax | Description |
|---------|--------|-------------|
| CREATE REPORT | `CREATE REPORT <file>` | Design report interactively |
| REPORT FORM | `REPORT FORM <file> [FOR <cond>] [TO PRINT]` | Run report |
| MODIFY REPORT | `MODIFY REPORT <file>` | Edit report design |

### Report File Format (.FRM)

Report definition stored as fixed-format binary:
- Page layout: width, length, margins, spacing
- Column definitions: header text, expression, width, decimals
- Group breaks: expression, header text, sub-total flag
- Summary: grand totals, record count

### Report Output

```
Page Header (title, date, page number)
Column Headers
---separator---
Detail lines (one per record)
  [Group break: subtotals when group expression changes]
---separator---
Summary (grand totals)
```

## Label Generator

| Command | Syntax | Description |
|---------|--------|-------------|
| CREATE LABEL | `CREATE LABEL <file>` | Design label |
| LABEL FORM | `LABEL FORM <file> [FOR <cond>] [TO PRINT]` | Print labels |

Standard label sizes (Avery compatible):
- 3.5" x 15/16" (1-across)
- 3.5" x 15/16" (2-across)
- 4" x 1-7/16"

## Source Files

```
src/
  ndx.c           # B-tree index implementation
  ndx.h
  report.c        # Report generator
  report.h
  label.c         # Label generator
  label.h
```

## Success Criteria

- `INDEX ON name TO names.ndx` creates working B-tree
- `SEEK "SMITH"` finds record in O(log n) time
- `LIST FOR name = "SMITH"` uses index automatically
- Record insertion/update maintains index integrity
- REINDEX rebuilds correctly after bulk operations
- REPORT FORM produces formatted columnar output with group breaks
- LABEL FORM prints multi-column mailing labels
