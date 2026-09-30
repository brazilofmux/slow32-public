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

The runtime's own items, to do:

- the 01004 truncation warning, with the full length in the indicator;
- a finer SQLSTATE map (22019 for a bad LIKE escape, 22003 and 22012).

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
   variables), CONNECT.
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
