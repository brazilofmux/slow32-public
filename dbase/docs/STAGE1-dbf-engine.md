# Stage 1: DBF Engine + Minimal REPL

## Goal

Read and write dBase III `.DBF` files and provide a dot-prompt
interpreter with enough commands to create a database, add records,
modify fields, list data, and quit. This is the foundation everything
else builds on.

## Deliverables

1. **DBF library** (`dbf.h` / `dbf.c`) — open, create, read, write, append, close
2. **Dot prompt REPL** (`main.c`) — command parsing and dispatch
3. **Field access helpers** (`field.h` / `field.c`) — get/set typed field values
4. **Build script** (`build.sh`)
5. **Test suite** (`tests/`)

## DBF Library API

```c
// Core handle
typedef struct dbf_handle dbf_t;

// Field descriptor (exposed to callers)
typedef struct {
    char name[12];      // Field name (NUL-terminated)
    char type;          // 'C', 'N', 'D', 'L'
    uint8_t length;     // Field width
    uint8_t decimals;   // Decimal places (N fields only)
    uint16_t offset;    // Byte offset within record (computed)
} dbf_field_t;

// File operations
dbf_t *dbf_open(const char *filename, const char *mode);  // "r", "rw"
dbf_t *dbf_create(const char *filename, const dbf_field_t *fields, int nfields);
void   dbf_close(dbf_t *db);

// Record I/O
int  dbf_read_record(dbf_t *db, int recno);    // Load into internal buffer
int  dbf_write_record(dbf_t *db);              // Flush current record
int  dbf_append_blank(dbf_t *db);              // Append empty record
int  dbf_record_count(dbf_t *db);
int  dbf_current_record(dbf_t *db);

// Field access (operate on current record buffer)
const char *dbf_get_field(dbf_t *db, int fieldno);       // Raw ASCII
void  dbf_set_field(dbf_t *db, int fieldno, const char *value);
int   dbf_field_index(dbf_t *db, const char *name);      // -1 if not found
const dbf_field_t *dbf_field_info(dbf_t *db, int fieldno);
int   dbf_field_count(dbf_t *db);

// Header management
int  dbf_flush_header(dbf_t *db);   // Update record count + date
```

## Commands (Stage 1)

| Command | Syntax | Description |
|---------|--------|-------------|
| CREATE | `CREATE <filename>` | Interactive field definition |
| USE | `USE <filename>` / `USE` | Open/close database |
| APPEND BLANK | `APPEND BLANK` | Add empty record |
| REPLACE | `REPLACE <field> WITH <value> [, ...]` | Set field values |
| LIST | `LIST` / `LIST <fields>` | Show all records |
| DISPLAY | `DISPLAY` | Show current record |
| DISPLAY STRUCTURE | `DISPLAY STRUCTURE` / `DISPLAY STRU` | Show field layout |
| ? | `? <expression>` | Print value (literals only in stage 1) |
| QUIT | `QUIT` | Exit |

## Source Files

```
dbase/
  build.sh          # Build script (like sbasic/build.sh)
  src/
    main.c          # Entry point + REPL loop
    command.c        # Command parser + dispatch
    command.h
    dbf.c           # DBF file format engine
    dbf.h
    field.c         # Typed field access (get_string, get_number, etc.)
    field.h
    util.c          # String helpers (trim, upper, etc.)
    util.h
  tests/
    run-tests.sh
    test_create.txt  # CREATE + DISPLAY STRU
    test_append.txt  # USE + APPEND + REPLACE + LIST
    test_types.txt   # All four field types
```

## Implementation Notes

- Records are 1-based (dBase convention). Record 0 is invalid.
- All field data stored as space-padded ASCII. Numeric fields are
  right-justified; character fields are left-justified.
- Date fields stored as `YYYYMMDD` (8 characters, no separators).
- Logical fields: `T`, `F`, `Y`, `N`, `?` (uninitialized = space).
- The deletion flag is byte 0 of each record: `' '` = active, `'*'` = deleted.
- CREATE in Stage 1 can be a simple interactive prompt (no full-screen editor).
- REPLACE WITH accepts string literals (`"..."` or `'...'`), numeric literals,
  date literals (`{MM/DD/YY}`), and logical literals (`.T.` / `.F.`).
- LIST outputs one record per line: field values separated by spaces.

## Testing Strategy

Each test is a text file piped as stdin:
```bash
(cat tests/test_create.txt; echo "QUIT") | \
  ../tools/emulator/slow32-fast dbase.s32x
```

Expected output is compared against `tests/expected/` files.

## Success Criteria

- Can CREATE a database with C, N, D, L fields
- Can USE it, APPEND BLANK, REPLACE fields, LIST records
- Can close and reopen — data persists across sessions
- DISPLAY STRUCTURE shows correct field layout
- DBF files are binary-compatible with real dBase III
