# Embedded SQL

## Status

Phase 1, 2026-09-29. `tests/nist-sql-run.sh` (harness gate 5b, 22
seconds) against the NIST embedded COBOL programs:

| | count |
|---|---|
| programs | 429 |
| compile and run to the end | 346 |
| tests passing | 356 |
| tests failing | 326 |
| do not compile | 83, all phase 2 or 3 statements (dynamic SQL, SET, GET DIAGNOSTICS, INCLUDE, WHENEVER, scroll cursors) |
| not run | 14: the concurrent mpa/mpb pairs and the one C program |
| schema elements SQLite refuses | 35 of the suite's DDL |

The schema refusals:

- WITH CHECK OPTION on views;
- views over another schema's tables ("cannot reference objects in
  database");
- REFERENCES to a table in another schema;
- INTERVAL types and CHARACTER SET;
- some query shapes.

Found on the way:

- SQLite starts a query over when stepped past SQLITE_DONE; the runtime
  keeps an exhausted cursor at no data.
- The user special values are rewritten to the user's name.
- The suite's source shapes the compiler now takes:
  - SQL strings split across lines, continued by `"` or by nothing;
  - CHARACTER SET [IS] name on host variables;
  - a literal over 160 positions (BP-E20);
  - EXIT PROGRAM followed by STOP RUN (BP-E21).
- SQLite is built with SQLITE_MAX_ATTACHED=125 for the 18 schemas.

What the 326 failures are (COB_SQL_TRACE=1 on the gate's run). Nearly
all are SQLite itself:

- **Views are read-only.** 557 INSERTs into TESTREPORT, the view every
  program records its results through. The pass/fail counts come from the
  program's output, so they are unaffected.
- **Missing SQL-92 syntax:**
  - DROP ... CASCADE/RESTRICT, 132 statements. Each leaves its table
    behind for the next test's CREATE;
  - CREATE DOMAIN;
  - quantified comparisons (> ALL, ANY);
  - EXTRACT, interval and datetime literals, OVERLAPS;
  - ALTER TABLE ADD (a, b);
  - parenthesized compound SELECTs;
  - NATURAL JOIN over schema-qualified names.
- **No INFORMATION_SCHEMA** (about 600 references).
- **No rowid in a view or a join**, where a positioned statement needs one.

Done since:

- **Character columns compare blank-padded.** A character column's type
  in CREATE TABLE and ALTER TABLE ADD gets SQLite's own `COLLATE RTRIM`,
  in the runtime and in the schema loader alike. SQL-92 compares every
  character type under a PAD SPACE collation, so `k = 'AB   '` finds
  'AB', literal against column as well as host variable.
- **A normal end commits** (STOP RUN, or GOBACK from the main program),
  as DB2 does; ISO leaves it to the implementation. libcob's
  `cob_at_stop` hook is set by the SQL runtime when it first connects.

- **Truncation and range.**
  - A character value cut short on the way to its host variable is the
    warning 01004, with SQLCODE 0 and the value's length in the indicator.
  - A number with too many integer digits for its host variable is
    22003 (-304), the host variable unchanged; before, it was silently
    truncated as a MOVE would.
  - SQLite's words map to SQL-92's conditions where they match: 22019
    for a bad LIKE escape, 22003 for integer overflow.

- **Phase 2, the DB2 conventions** (tests/fixed/esqldb2):
  - **INCLUDE member** works as COPY does. `INCLUDE SQLCA`, when the
    program has no copybook of that name, is DB2's layout with BINARY for
    COMP-5. In the LINKAGE SECTION it has no VALUE clauses. When the
    program declares its own SQLCODE or SQLSTATE, the SQLCA leaves that
    one to the program.
  - **The SQLCA is filled by name** after each statement: SQLCODE,
    SQLSTATE, SQLERRML/SQLERRMC with the backend's message, SQLERRD(3)
    with the rows touched, SQLWARN0 and SQLWARN1.
  - **WHENEVER** SQLERROR (SQLCODE < 0), SQLWARNING (SQLSTATE class 01)
    or NOT FOUND takes CONTINUE or GO TO, from where it stands to the end
    of the unit.
  - **A host structure** (:group) is its elementary items, in order.
  - **DECLARE ... TABLE** (DCLGEN) is accepted and does nothing.
  - **CONNECT** [TO t] [USER u | :u] and Oracle's `CONNECT :u IDENTIFIED
    BY :p` name the authorization id. CONNECT RESET and DISCONNECT close
    the connection.

- **Phase 3, dynamic SQL, less the descriptors:**
  - EXECUTE IMMEDIATE, of a host variable or a literal;
  - PREPARE name FROM, then EXECUTE name [INTO ...] [USING ...]. A
    statement that returns a row gives it to INTO, as SELECT INTO does;
  - DECLARE c CURSOR FOR name, and OPEN c USING. When such a cursor is
    used positioned, the runtime prepares its SELECT with rowid first;
  - DEALLOCATE PREPARE;
  - GET DIAGNOSTICS: NUMBER, MORE, COMMAND_FUNCTION, DYNAMIC_FUNCTION and
    ROW_COUNT; and, for EXCEPTION n, RETURNED_SQLSTATE, MESSAGE_TEXT,
    MESSAGE_LENGTH, CLASS_ORIGIN, SUBCLASS_ORIGIN and CONDITION_NUMBER.
    Items SQLite has nothing for come back blank or zero;
  - SET SESSION AUTHORIZATION changes the user, outside a transaction.
    SET TRANSACTION, CONSTRAINTS, TIME ZONE, CATALOG, SCHEMA and NAMES
    succeed and do nothing (behavior points: SQLite is serializable and
    read-write, and has none of the rest);
  - delimited cursor names ("A < a").

  The suite's COBOL needed two more changes:
  - `--` inside a COBOL host name (:CITY1---city1) is no SQL comment;
  - a separator comma with no space after it is BP-E22.

- **SQL descriptors** (tests/free/esqldesc; esqldyn has the rest):
  - ALLOCATE DESCRIPTOR [WITH MAX n | :h] and DEALLOCATE;
  - SET DESCRIPTOR COUNT, and for VALUE n: TYPE (which resets the
    item), LENGTH, OCTET_LENGTH, PRECISION, SCALE, NULLABLE, INDICATOR,
    NAME and DATA. DATA is taken as the item's TYPE says;
  - GET DESCRIPTOR, those fields and RETURNED_LENGTH;
  - DESCRIBE [INPUT | OUTPUT] from SQLite's declared column types. An
    expression has none, so it is NUMERIC and UNNAMED, and nullability
    is not known (a behavior point);
  - USING SQL DESCRIPTOR on EXECUTE and OPEN, INTO SQL DESCRIPTOR on
    EXECUTE and FETCH.

  Found on the way: the owner's qualifier was rewritten to `main.`, and
  a view created with it stored main.X in its text. SQLite then refused
  to attach that schema under its own name ("cannot reference objects in
  database main"), and every later program lost the schema. The
  qualifier is now dropped instead, in the runtime and in the loader.
  The runner prints every program's counts, so a program that stops
  printing shows in a diff.

  Not yet: scroll cursors (FETCH PRIOR/FIRST/LAST/ABSOLUTE: SQLite
  cursors go forward only).

  Floating-point host variables (COMP-1 without a PICTURE, COMP-2)
  arrived with the floats (docs/usage.md, 2026-09-30). They bind as
  REAL and fetch the column's double. dml035 compiles and passes: 421
  programs compile, 386 tests pass.

What SQLite cannot report, so no map will reach:

- 22012, division by zero: it yields NULL;
- 21000 from a scalar subquery with several rows: it takes the first;
- 01003, null eliminated in a set function;
- 22001, a string too long for its column: SQLite has no length;
- 44000, check option;
- 22025, an invalid escape sequence.

NATURAL JOIN lists its columns in SQLite's order, not SQL-92's (the
common columns first), so `SELECT *` over one fills the wrong host
variables.

## The plan

Started 2026-09-29. The plan for EXEC SQL in s32-cobc, with SQLite
running inside the guest as the database. Two rulings from the user
shaped it:

- **SQLite as it is.** Where its semantics differ from SQL-92, the
  difference is recorded as a behavior point against the NIST results,
  not worked around. The runtime talks to the database through a backend
  interface, so a stricter backend can be added later.
- **ISO/NIST first.** Start with what the NIST suite uses. The DB2
  conventions real legacy code carries (INCLUDE SQLCA, WHENEVER,
  indicator variables, host structures) come next.

## The authority: the NIST SQL Test Suite, Version 6.0

The embedded-SQL counterpart of CCVS-85. NIST, NCC (UK) and Computer
Logic R&D, 1996; Entry and Intermediate SQL-92 (ISO/IEC 9075:1992)
through each standard host language. Its download page is gone.

The zips survive in the Internet Archive and are unpacked, outside any
git tree, in `~/refs/nist-sql`:

- `v6pcisql.zip`: the base unit (schemas, interactive SQL, embedded C,
  the User's Guide);
- `v6_pco.zip`: embedded COBOL, 453 programs.

Parts are copyrighted by NCC and Computer Logic, so the suite is never
copied into the tree.

What the COBOL unit uses:

- ISO embedded SQL: host variables in BEGIN/END DECLARE SECTION, and a
  program-declared SQLCODE or SQLSTATE item (no SQLCA);
- `:host` references, cursors, SELECT INTO, INSERT/UPDATE/DELETE and
  COMMIT/ROLLBACK;
- dynamic SQL: PREPARE, EXECUTE, DESCRIBE and descriptors;
- GET DIAGNOSTICS, DDL, and GRANT;
- `CALL "AUTHID"`, the implementor's login routine.

`runpco.all` gives the order and each program's authorization id.
Every test prints `*** pass ***` or `*** fail ***` and records a row in
TESTREPORT.

## The shape

**The compiler does the precompiling.** The tokenizer takes
`EXEC SQL ... END-EXEC` as one token holding the statement's text, so
SQL never meets COBOL tokenizing. The parser handles that token in each
division:

- In the data division:
  - DECLARE SECTION markers are accepted and ignored: any item may be a
    host variable;
  - INCLUDE is phase 2.
- In the procedure division:
  - a statement is scanned for `:name` host references (a name, `:a.b`
    qualification, an indicator);
  - each reference is replaced by `?`;
  - the reference is classified as input, or as output (an INTO list).
  - The statement becomes a static descriptor in .data: the rewritten
    text, the input list and the output list (item address, cob_desc,
    indicator), and a slot for the prepared statement.
- The emitted code:
  1. calls the runtime;
  2. copies the result into the program's SQLCODE and SQLSTATE items,
     if it declared them;
  3. under WHENEVER, branches (phase 2).
- Cursors: DECLARE CURSOR emits nothing and records the query. OPEN
  binds its inputs, FETCH receives the outputs, CLOSE resets.

**The runtime is `libcob/esql.c`, a separate object.** A program
without SQL does not pull SQLite in. `compile.sh` links it, with
`sqlite/out/libsqlite3.s32a` and a 2MB code segment, when the compiler
reports SQL in the program. The runtime:

- keeps the connection;
- prepares each statement on first use and caches it in its descriptor;
- binds and fetches through cob_desc conversions;
- maps the backend's outcome to SQLCODE and SQLSTATE.

The backend is a table of functions (open, prepare, bind, step, column,
reset, finalize, error) behind that runtime. SQLite is the first
implementation.

**Values between COBOL and SQLite:**

| from | to | how |
|---|---|---|
| numeric item, scale 0, fits 64 bits | SQLite | integer |
| other numeric item | SQLite | its exact decimal text; the column's affinity decides the storage |
| alphanumeric item | SQLite | text, trailing spaces trimmed (SQL-92 compares CHAR blank-padded; SQLite compares exactly, and a trimmed value compares like a padded one against what literals inserted) |
| SQLite | numeric item | the value's text, read as a decimal |
| SQLite | alphanumeric item | text, by the MOVE rules |

NULL into an item with no indicator gives SQLCODE -305 and SQLSTATE
22002.

**Schemas.** SQLite has no CREATE SCHEMA and no users. So:

- each schema, one per authorization id, is a database file in the
  program's data directory (`COB_SQL_DIR`, default `.`), named
  `<schema>.db`;
- CALL "AUTHID" (or CONNECT) opens the user's schema file as `main` and
  attaches the others under their own names;
- `HU.ECCO` from user HU is rewritten lexically, outside string
  literals, to `main.ECCO`;
- `CREATE SCHEMA AUTHORIZATION X` followed by its elements is split
  into X's statements;
- GRANT and REVOKE succeed and do nothing (a behavior point: SQLite has
  no privileges).

**SQLCODE and SQLSTATE:**

| outcome | SQLCODE | SQLSTATE |
|---|---|---|
| success | 0 | 00000 |
| no data | 100 | 02000 |
| constraint violation | -803 or -530 | 23000 |
| any other error | -1 | the class that fits (42000 syntax, 22xxx data) |

## Phases

1. **Static SQL, enough for the NIST data loaders and the DML programs:**
   - the EXEC SQL token, and host references;
   - SELECT INTO, INSERT, UPDATE, DELETE, searched and positioned
     (WHERE CURRENT OF);
   - cursors, COMMIT and ROLLBACK;
   - SQLCODE and SQLSTATE;
   - AUTHID and schemas, DDL passed through, GRANT as a no-op.

   The harness `tests/nist-sql-run.sh` loads the schemas, runs
   `runpco.all` in order under each authorization id, and totals passes
   and fails as ccvs-run.sh does. A baseline file gates it.
2. **DB2 conventions:** INCLUDE SQLCA (and INCLUDE of a member), WHENEVER,
   indicator variables, host structures (a group as a list of host
   variables), CONNECT. Done 2026-09-30.
3. **Dynamic SQL:** EXECUTE IMMEDIATE, PREPARE and EXECUTE, DESCRIBE,
   ALLOCATE and GET/SET DESCRIPTOR; GET DIAGNOSTICS.
4. **The rest of Intermediate SQL, as far as SQLite goes.** Every failure
   left is a behavior point: SQLite's semantics, recorded, not emulated.

## Not planned

- A SQL parser. The compiler finds host references and statement kinds
  and passes the rest through.
- Emulating SQL-92 where SQLite differs. The differences expected:
  - DECIMAL stored as a double, exact to 15 digits;
  - CHAR compared exactly (the trimming above covers host variables,
    not literal-to-column comparisons);
  - views that reference another schema's tables;
  - domains, privileges, and deferred constraints.
