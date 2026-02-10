# dBase III Clone — Roadmap

Prioritized work plan organized into phases. Each phase is independently
committable and testable.

## Phase 1: Teacher's Pet Unblocking --- DONE

**Goal**: Get sane.prg and tp.prg to run without errors.
**Effort**: Small (1 session). **COMPLETE** (45/45 tests).

### 1a. Accept missing SET options as no-ops

sane.prg (the first thing Teacher's Pet runs) uses SET options we
don't recognize. The parser must accept them silently:

- `SET ECHO ON/OFF`
- `SET MENU ON/OFF` (or `SET MENUS`)
- `SET STATUS ON/OFF`
- `SET SCOREBOARD ON/OFF`
- `SET FUNCTION <n> TO <string>`

Implementation: Add cases to set.c that parse the ON/OFF or arguments
and discard them. No state storage needed — these control features
we don't have (program tracing, menu bar, status line, function keys).

### 1b. CLOSE DATABASES

`CLOSE DATABASES` closes all open databases in all work areas.
tp.prg uses this for cleanup.

Implementation: In command.c, match `CLOSE DATABASES` (or abbreviation
`CLOSE DATA`) and call `cmd_close_all()`.

Also add: `CLOSE ALL` (same as CLOSE DATABASES plus releases memory
variables and closes procedure file). `CLOSE INDEX` (close indexes in
current area only).

### 1c. RELEASE ALL LIKE / EXCEPT

`RELEASE ALL LIKE T_*` releases all memory variables whose names match
the pattern. Teacher's Pet uses this for cleanup.

Implementation: Add `release_matching(pattern, like_mode)` to memvar.c.
Pattern matching is simple: `*` matches any suffix, `?` matches one char.
dBase III only supports trailing `*` (e.g., `T_*`), not full glob.

### 1d. Date arithmetic verification

Teacher's Pet does `TE_DATE - CTOD("01/01/80")` to get days-since-epoch
as a numeric value for index keys. Verify our expression evaluator
returns a numeric result for date minus date. If not, add it.

Also verify: date + number = date, date - number = date.

---

## Phase 2: Screen & Output Completeness --- DONE

**Goal**: Teacher's Pet menus and reports display correctly.
**Effort**: Medium (1-2 sessions). **COMPLETE** (46/46 tests).

### 2a. SET DEVICE TO SCREEN/PRINT

This is how dBase III routes `@SAY` output to the printer. When
`SET DEVICE TO PRINT`, all `@row,col SAY` output goes to the printer
device instead of the screen. `@GET` always goes to screen regardless.

For SLOW-32: "printer" means stdout (same as screen). The key behavior
difference is that SET DEVICE TO PRINT suppresses cursor positioning —
output is sequential. SET DEVICE TO SCREEN restores normal positioning.

Implementation:
- Add `device` field to set_state (SCREEN or PRINT)
- In screen.c `@SAY` handler, check device. If PRINT, use printf
  instead of cursor positioning. Track printer row/col for `PROW()`/`PCOL()`.

### 2b. @ GET ... RANGE lo,hi

Add RANGE clause parsing to the GET command in screen.c. During READ,
after the user enters a value, validate it falls within the range.
If not, beep and force re-entry.

`@ 6,39 GET T_quar PICTURE "#" RANGE 0,9`

Implementation: Add `range_lo`, `range_hi`, `has_range` fields to
`get_entry_t`. Parse after PICTURE (or instead of PICTURE). Check
during READ confirm.

### 2c. ROW() and COL() functions

Return current cursor position. Used by Teacher's Pet for dynamic
screen layout (`currow = ROW()`).

Implementation: Track cursor position in screen.c (updated by every
@SAY and @GET). Add ROW() and COL() to func.c that query screen state.

### 2d. SET COLOR TO with intensity modifiers

Parse `W+` (high-intensity white), `W*` (blinking), `GR+/R` (bright
yellow on red). The color spec format is:

`SET COLOR TO <fg>[+|*]/<bg>[+|*]`

Where letters are: N(black), B(blue), G(green), BG(cyan), R(red),
RB(magenta), GR(brown/yellow), W(white). `+` = bright, `*` = blink.

In fallback mode (no terminal), intensity modifiers are ignored but
must be parsed without error.

### 2e. PROW() and PCOL() functions

Current printer row and column. Used by some report programs.
Implementation: Track in set/screen state, return from func.c.

---

## Phase 3: Remaining dBase III Functions

**Goal**: Complete function library for broad compatibility.
**Effort**: Small-Medium (1 session).

### Missing functions needed for general compatibility

| Function | Description | Priority |
|----------|-------------|----------|
| IIF(cond,t,f) | Inline if — very commonly used | High |
| FILE(filename) | Test if file exists | High |
| DTOS(date) | Date to YYYYMMDD string (for index keys) | High |
| STUFF(s,start,del,ins) | Insert/replace in string | Medium |
| TRANSFORM(expr,pic) | Format value with PICTURE | Medium |
| ISALPHA(s) | First char is letter? | Low |
| ISUPPER(s) / ISLOWER(s) | Case test | Low |
| SOUNDEX(s) | Phonetic code | Low |
| VERSION() | Return version string | Low |
| OS() | Return OS string | Low |
| INKEY([secs]) | Check keyboard | Low |
| LASTKEY() | Last key pressed | Low |
| READKEY() | Key that exited READ | Low |
| ERROR() / MESSAGE() | Error info | Low |
| LINENO() / PROGRAM() | Debug info | Low |

### Missing operators

| Operator | Description | Priority |
|----------|-------------|----------|
| `**` or `^` | Exponentiation | Medium |
| `==` | Exact string equality (ignores SET EXACT) | Medium |
| `-` (string) | Trim-concatenate ("AB  " - "CD" = "ABCD  ") | Low |
| `!` | Alternative for .NOT. | Low |

---

## Phase 4: Data Commands

**Goal**: Complete the data manipulation command set.
**Effort**: Medium (1-2 sessions).

### 4a. SET RELATION TO

`SET RELATION TO <expr> INTO <alias>` links two work areas so that
when the parent moves, the child automatically seeks to the matching
record. Teacher's Pet doesn't use this directly (it does manual SEEKs),
but it's a core dBase III feature.

### 4b. JOIN WITH

`JOIN WITH <alias> TO <file> FOR <cond> [FIELDS <list>]`
Combine records from two databases into a new file. Not commonly used
but part of the dBase III spec.

### 4c. UPDATE ON

`UPDATE ON <key> FROM <alias> REPLACE <field> WITH <expr>`
Batch update from another database. Not commonly used.

### 4d. TOTAL ON

`TOTAL ON <key> TO <file> [FIELDS <list>]`
Create summary database with totals per key value.

### 4e. SAVE TO / RESTORE FROM

`SAVE TO <file> [ALL LIKE/EXCEPT <pattern>]`
`RESTORE FROM <file> [ADDITIVE]`
Save/load memory variables to/from .MEM files. Not used by Teacher's
Pet but part of the dBase III spec.

### 4f. DISPLAY/LIST enhancements

- `TO PRINT` clause (send output to printer)
- `TO FILE <filename>` clause (send output to file)
- `OFF` clause (suppress record numbers) — may already work

---

## Phase 5: Error Handling & Robustness

**Goal**: Programs survive bad data and user mistakes gracefully.
**Effort**: Medium.

### 5a. ON ERROR DO <procedure>

Install an error handler. When a command fails (file not found,
type mismatch, etc.), instead of printing an error and aborting,
call the error procedure. That procedure can use ERROR(), MESSAGE(),
LINENO(), PROGRAM() to decide what to do.

### 5b. Improved error messages

Current errors are terse. Add file/line context when executing programs.
Example: `Error in TPPG.PRG line 42: Variable not found: T_FOO`

### 5c. SUSPEND / RESUME

SUSPEND pauses program execution and drops to the dot prompt for
debugging. RESUME continues from where we left off. Useful for
development but not critical for Teacher's Pet.

---

## Phase 6: Report & Label Generators

**Goal**: REPORT FORM and LABEL FORM commands.
**Effort**: Large.

These parse binary .FRM and .LBL definition files and generate
formatted output. Teacher's Pet's tp.prg has a `REPORT FORM` call
but doesn't appear to depend on it for core functionality.

This is the largest remaining feature and could be deferred until
all other phases are complete. An alternative is to implement a
simplified report command that works with a text-based format
definition rather than the binary .FRM format.

---

## Phase 7: B-Tree Indexing (Performance)

**Goal**: Replace sorted-array indexes with on-disk B-tree.
**Effort**: Large.

The current in-memory sorted array works fine for Teacher's Pet
workloads (hundreds of records). B-tree only becomes necessary for
databases with thousands of records or for NDX file format
compatibility with real dBase III.

This is a performance optimization, not a correctness issue. Defer
until Teacher's Pet is fully functional.

---

## Verification Strategy

### Unit tests (automated)

Continue the existing pattern: pipe commands to dbase.s32x, compare
output to expected files. Add tests for each new feature.

### Integration test: Teacher's Pet

The ultimate acceptance test. Steps:
1. Copy akron/*.PRG and akron/*.DBF to a test directory
2. Pipe tp.prg startup sequence to the clone
3. Exercise each menu option
4. Compare behavior to expected (documented in akron/readme.md)

### Incremental testing

After each phase, run the full test suite plus try loading sane.prg
and tp.prg. Track progress: how far does Teacher's Pet get before
hitting an unimplemented feature?

---

## Priority Summary

| Phase | What | Why | When |
|-------|------|-----|------|
| 1 | Unblocking | sane.prg must parse cleanly | Next |
| 2 | Screen/output | Menus and reports must display | After 1 |
| 3 | Functions | IIF, FILE, DTOS commonly needed | After 2 |
| 4 | Data commands | SET RELATION, JOIN (less common) | After 3 |
| 5 | Error handling | ON ERROR for robustness | After 4 |
| 6 | Reports | REPORT FORM (binary format) | After 5 |
| 7 | B-tree | Performance for large databases | After 6 |
