# Stage 3: Variables & Commands

## Goal

Add memory variables, SET options, work areas (multiple open databases),
and bulk data commands (COPY, APPEND FROM, SORT, DELETE/PACK).

## Prerequisites

- Stage 2 complete (navigation + expressions)

## Memory Variables

```c
// Up to 256 memory variables (dBase III limit)
typedef struct {
    char name[12];
    value_t value;
    bool is_public;
} memvar_t;
```

| Command | Syntax | Description |
|---------|--------|-------------|
| STORE | `STORE <expr> TO <var>` | Assign variable |
| = | `<var> = <expr>` | Shorthand assign |
| RELEASE | `RELEASE <var> [, ...]` / `RELEASE ALL [LIKE/EXCEPT <skel>]` | Free variables |
| DISPLAY MEMORY | `DISPLAY MEMORY` | Show all variables |
| SAVE TO | `SAVE TO <file> [ALL LIKE/EXCEPT <skel>]` | Save vars to .MEM |
| RESTORE FROM | `RESTORE FROM <file> [ADDITIVE]` | Load vars from .MEM |

Variables are accessible in expressions: `? mTotal * 1.08`

## SET Commands

| SET Option | Values | Default | Effect |
|------------|--------|---------|--------|
| TALK | ON/OFF | ON | Echo command results |
| DELETED | ON/OFF | OFF | Hide deleted records |
| EXACT | ON/OFF | OFF | Exact string matching |
| CONFIRM | ON/OFF | OFF | Require Enter after GET |
| BELL | ON/OFF | ON | Beep on errors |
| HEADING | ON/OFF | ON | Column headers in LIST |
| SAFETY | ON/OFF | ON | Confirm overwrites |
| CONSOLE | ON/OFF | ON | Screen output |
| ALTERNATE | TO file / ON / OFF | - | Log output to file |
| DATE | AMERICAN/ANSI/BRITISH/etc. | AMERICAN | Date display format |
| DECIMALS | TO n | 2 | Decimal display precision |
| DEFAULT | TO drive | - | Default drive/path |

## Work Areas

dBase III supports 10 work areas (1-10, aliased A-J).

| Command | Syntax | Description |
|---------|--------|-------------|
| SELECT | `SELECT <area/alias>` | Switch work area |
| USE...IN | `USE <file> IN <area>` | Open in specific area |
| ALIAS | Automatic from filename | Reference: `alias->field` |

Cross-area field references: `B->name` accesses field `name` in area B.

## Bulk Data Commands

| Command | Syntax | Description |
|---------|--------|-------------|
| DELETE | `DELETE [scope] [FOR <cond>]` | Mark records deleted |
| RECALL | `RECALL [scope] [FOR <cond>]` | Unmark deleted records |
| PACK | `PACK` | Physically remove deleted records |
| ZAP | `ZAP` | Delete all records |
| COPY TO | `COPY TO <file> [FIELDS ...] [FOR <cond>]` | Export records |
| COPY STRUCTURE | `COPY STRUCTURE TO <file>` | Copy empty structure |
| APPEND FROM | `APPEND FROM <file> [FOR <cond>]` | Import records |
| SORT | `SORT TO <file> ON <field> [/A/D/C]` | Sort to new file |

## Scope Clauses

Commands that iterate records accept scope:
- `ALL` — all records (default for LIST, COUNT, etc.)
- `NEXT <n>` — next n records from current position
- `RECORD <n>` — single record
- `REST` — from current position to end

## I/O Commands

| Command | Syntax | Description |
|---------|--------|-------------|
| ACCEPT | `ACCEPT ["prompt"] TO <var>` | String input |
| INPUT | `INPUT ["prompt"] TO <var>` | Expression input |
| WAIT | `WAIT ["prompt"] [TO <var>]` | Single keypress |

## Source Files

```
src/
  memvar.c        # Memory variable store
  memvar.h
  workarea.c      # Work area management (10 areas)
  workarea.h
  set.c           # SET option management
  set.h
  bulk.c          # COPY, APPEND FROM, SORT, PACK
  bulk.h
```

## Success Criteria

- `STORE "Hello" TO x` then `? x` prints `Hello`
- `SELECT 2` / `USE orders` / `SELECT 1` / `? B->total` works
- `DELETE FOR amount < 0` / `PACK` removes records
- `COPY TO backup FOR state = "CA"` creates filtered copy
- `SET DELETED ON` hides deleted records from LIST
- `APPEND FROM other.dbf` imports records
