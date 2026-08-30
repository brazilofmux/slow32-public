# COBOL 85 front (`cobol/`) — open items and post-mortems

The in-tree engineering log for `s32-cobc` and `libcob`, kept next
to the code (CLAUDE.md: cite an entry as **`cobol ISSUES-N`**, never a
bare `#N`). The stage history is [docs/plan.md](docs/plan.md); the
measured corpus table is [docs/majesty-corpus.md](docs/majesty-corpus.md);
this file is what is *open*, ranked, plus what was closed and why.
Nothing here is scheduled: the front is app-driven, and an item moves
when a program asks for it.

State on 2026-08-30: harness 46/46 with the GnuCOBOL oracle agreeing
on every program that has one; majesty `batch.sh` runs every COBOL
report step on SLOW-32 with all twelve reports byte-identical; 42 of
the 58 programs in `~/majesty/src/cobol` compile. The sweep that
measures the last number is one line, run from `~/majesty`:

    for f in src/cobol/*.cbl; do ~/slow-32/cobol/out/s32-cobc -free -m -I src/copy -o /dev/null $f; done

## A. The corpus — 14 refusals, by what unblocks most

### 1. ~~RELATIVE I-O (3 programs: crglentry, ldglentry, exglentry)~~ — RESOLVED 2026-08-30
Stage 19. Slots of `4 + recsize` framed with the mode-V RDW (zero =
empty), which also carries glentry's variable-length records; the
six verbs under random, dynamic and sequential access, statuses and
positioning measured against GnuCOBOL (docs/indexed.md "As built").
crglentry and exglentry run on SLOW-32 with GnuCOBOL's output;
ldglentry now stops at its `SD` (ISSUES-4). The on-disk bytes differ
from GnuCOBOL's 8-byte native length -- documented, and no program
outside COBOL reads these files.

### 2. The legacy `FUNCTION-ID` date family (7 units + 2 callers)
`fielded_to_linear`, `linear_to_fielded`, `floor-div`, `floor-divmod`,
`holidays`, `isleapyear`, `isvaliddate` are COBOL 2002 user-defined
functions; `exgltrans` and `jerm` call them through `REPOSITORY`.
**User ruling: retire, do not rewrite** — the C `du_*` path in
`dateutil.c` replaced them (it is also what MVS 3.8j does, with
hand-written assembly). Work is on the majesty side: the two callers
move to `CALL 'du_...' USING` like the rest of the corpus did in
`1da955d`, and the seven units leave the build. Nothing in the
compiler. `bad/repository` pins the refusal message.

### 3. ~~`SPECIAL-NAMES` — `CLASS name IS '0' THROUGH '9'` (damm)~~ — RESOLVED 2026-08-30
Stage 16: a 256-entry membership table per class in the literal pool,
per program unit; the test beside `NUMERIC` in `parse_simple`;
`cob_class_user` in the runtime. damm then wanted console `ACCEPT`
(one line of stdin, moved as text) and `LENGTH OF`; both landed, and
damm's output is byte-identical to GnuCOBOL's over seven inputs
including the check-digit fixtures majesty's tests use. gl008's declarations were
unused and were removed on the majesty side. Other SPECIAL-NAMES
clauses (`CURRENCY SIGN`, `DECIMAL-POINT IS COMMA`, switches,
`SYMBOLIC`) stay refused by name until a program asks;
`DECIMAL-POINT IS COMMA` in particular touches the PICTURE scanner
and every literal.

### 4. `SD` and file `SORT` (glacpost; then ldglentry)
Sort-Merge module. `SORT sd-file ON ASCENDING KEY ... USING/INPUT
PROCEDURE ... GIVING/OUTPUT PROCEDURE`, `RELEASE`, `RETURN`. The
runtime half is a sort of fixed-length records on the key
descriptors (libc `qsort` with a comparator over `cob_desc` keys,
records spilled to a temp file only if the corpus ever needs more
than memory); the compiler half is the input/output procedure
control flow, which is a `PERFORM` range with `RELEASE`/`RETURN`
inside it. `glacpost` is the one program whose first refusal is this;
`ldglentry` reaches it after ISSUES-1. Table `SORT` (gl008, dist01)
is a different form — see ISSUES-19 / GitHub #10.

### 4a. ~~Table `SORT` (gl008, dist01) — GitHub #10~~ — RESOLVED 2026-08-30 by rewrite
Ruling: rewritten in majesty to COBOL 85 -- insertion sorts through a
holding element (stable; a 2002 table `SORT` leaves equal keys
unspecified). Under GnuCOBOL the old and new gl008 print twelve
receipts byte-identically; on SLOW-32 the same twelve match GnuCOBOL.
The same commit rewrote gl008's `ROUNDED MODE NEAREST-EVEN` and a
subscripted subscript (`cat-tax(ws-id(i))`, also 2002), and dist01's
`OCCURS UNBOUNDED` and 21-digit item (ISSUES-5).

### 5. ~~A numeric item of more than 18 digits (dist01)~~ — RESOLVED 2026-08-30 by rewrite
`s9(18)v999` became `s9(15)v999` in majesty; the compiler keeps the
standard's limit and names it.

### 6. ~~`FUNCTION INTEGER-OF-DATE` / `DATE-OF-INTEGER` (jerm2)~~ — RESOLVED 2026-08-30
Stage 18: the four calendar functions of the 1989 addendum
(`INTEGER-OF-DATE`, `DATE-OF-INTEGER`, `DAY-OF-INTEGER`,
`INTEGER-OF-DAY`), integer 1 = 1601-01-01, invalid input gives 0. A
result rides the intrinsic plumbing as numeric DISPLAY digits (ten for
a day count, eight for a date, seven for a day-of-year -- the widths
GnuCOBOL shows when the value is DISPLAYed directly). free/datefn agrees
with GnuCOBOL; jerm2 -- majesty's 400,000-day cross-check of the C
`du_*` routines against these functions -- compiles, runs on SLOW-32
in 0.4 s under the DBT, and reports no disagreement on either engine.

### 7. `USAGE BINARY-INT UNSIGNED` (testcrc)
GnuCOBOL extension (with `BINARY-LONG`, `BINARY-SHORT`, …). Rewrite
in majesty to `COMP-5 PIC 9(9)`, per the ruling; not a compiler item.

### 8. `XML` / `JSON` verbs (usexml, usejson)
GnuCOBOL extensions. Out of scope; the two are demonstration
programs. Retire from the build or leave them refusing — either way
not compiler work.

### 9. gl015, gl016 — a report field without a PICTURE
Both are retired programs, not in majesty's build. Not counted.
(The Stage 12+ corpus table used to list them under a subscripted
`SOURCE`; that refusal now belongs to live gl008 — ISSUES-19.)

### 19. ~~Subscripted `SOURCE` in a Report Writer field (gl008) — GitHub #9~~ — RESOLVED 2026-08-30
The field keeps the token position of its SOURCE reference and
`parse_ref` reads it at GENERATE, where every other reference is
parsed -- so subscripts, `OF` qualification and reference
modification all come with it (`nm(i)(1:3)` included). Test
free/rptsub prints straight out of an ODO table, GnuCOBOL agreeing.
gl008's next stop is `ROUNDED MODE` (2002, majesty rewrites to plain
`ROUNDED`), then its table `SORT` (ISSUES-4a / GitHub #10).

## B. Language — known gaps no program has asked for

### 10. Nucleus level 2 remainder
`MOVE/ADD/SUBTRACT CORRESPONDING` (each refused by name), abbreviated
combined relation conditions (`a > b and < c`), nested programs,
`REPLACE`, `COPY ... REPLACING` (refused by name), the full
`INSPECT CONVERTING` corner cases. Each is a diagnostic today, never
silence.

### 11. Report Writer: `CONTROL` breaks and `SUM`
The expensive half of the module. majesty's reports compute their
totals in the Procedure Division, so the page engine
(`docs/report-writer.md`) is enough for all twelve. Stage 7 chose
this deliberately; do not start it without a report that needs it.

### 12. Indexed: `ALTERNATE RECORD KEY`, `DUPLICATES`
`READ ... KEY IS` takes only the `RECORD KEY`. The `.key` side file
(`docs/indexed.md`) is one sorted table; alternates would be more
tables in the same file.

### 13. Screen: the user's eventual target
Recorded in `docs/screen.md` from RM COBOL / Micro Focus experience:
TAB order across fields with Enter as submit, numeric fields anchored
on the decimal point, `AUTO` (auto-tab when full), `SECURE` (`*`
masking), and fields drawn in reverse video or underline. As built
(Stage 8) the screen paints and accepts field by field. `UNDERLINE`
also needs the term service to grow an attribute (`open-questions.md`).

### 14. OCCURS DEPENDING ON is laid out at its maximum
Documented in `dialect.md`; a whole-group MOVE of an ODO group is
refused (`bad/odo-group-move`) rather than moved wrongly. The
standard's variable-length semantics (group size follows the
depending item) would matter for a group WRITE of an ODO record;
mode V WRITE already honours `dep_item` for the record length.

## C. Documented divergences from GnuCOBOL (not bugs — the text wins)

Kept in `docs/oracles.md` and `docs/dialect.md`, each with a
`.oracle-expected` beside the test that shows it:

- REWRITE of an absent key → status 23 (GnuCOBOL 21).
- WRITE of a record longer than `VARYING ... TO` → 44 (GnuCOBOL
  clamps and reports 00).
- Sequential mode V on disk carries the IBM RDW, length inclusive of
  the header (GnuCOBOL: a private length word, exclusive); measured
  by a `tapemgr` round trip on every V file the tests write.
- An over-long LINE SEQUENTIAL record → 04, the rest of the line
  dropped (GnuCOBOL 4 splits it into two records with 06).

## D. Harness and infrastructure

### 15. ~~The oracle vanished with the host GnuCOBOL~~ — RESOLVED 2026-08-30
GnuCOBOL was uninstalled from every host, and `run-tests.sh` chose
its oracle with `command -v cobc`, so the suite went on passing with
no oracle and said nothing. Now: `gnucobol:4.0-builder` (cobc) and
`gnucobol:4.0-runtime` (the built program) under podman or docker,
repo bind-mounted at its own path; the work directory moved under
`cobol/out/` because a podman machine on macOS cannot mount `/tmp`;
the last line names the oracle, or says `NO ORACLE`.

### 16. ~~`RESTORE.JCL` committed by accident~~ — RESOLVED 2026-08-30
`tapemgr create` writes a `RESTORE.JCL` into its working directory
(a real feature: the MVS job that restores the tape). The Stage 10
harness ran it from `cobol/`, and the file went in with `34d5a81e`.
Removed; the harness now runs tapemgr inside its work directory.

### 20. ~~A conditional branch past ±4096 bytes~~ — RESOLVED 2026-08-30
gl008's `100-allocation-reports` was the first PERFORM body longer
than a bcond can reach; the assembler refused the program ("Branch
offset out of range ... 4424 bytes away"). The compiler now keeps its
assembly in memory and relaxes: every instruction line it writes is
one 4-byte instruction (`li`/`la` are already spelled out), so .text
positions are exact, and a branch that cannot reach becomes its
inverse over a `jal` (±1 MB), iterated to a fixed point. gl008 needs
four; tests/free/farbranch two. Found only because the corpus's
biggest program finally compiled -- the sweep's value again.

### 17. CCVS-85 as a histogram
The NIST suite (NC, SQ, IC modules) has never been run through
`s32-cobc`. First use is a *histogram of first refusals*, like the
majesty sweep — not a pass/fail gate, which would need the
implementor-defined parts (`X-cards`) settled first.

### 18. Building on a host without LLVM
`cctool.sh` (b96c4aff, Kagura) falls back to the self-hosted stage08
`cc.s32x` under the emulator when `$LLVM_BIN/clang` is absent. That
route exposed a stage08 parser gap — a block-scope declarator list
ending at a brace initializer — filed as GitHub #8, worked around in
`libcob.c` (957b5a29), and fixed in the parser on 2026-08-30
(`parse_local_declarator`; stage08 `tests/test_phase32.c`; the unsplit
`libcob.c` compiles again). The kit `~/s32x/cc.s32x` carries the fix
only once the kit is rebuilt. The fallback has been exercised only for
`libcob.c`; a `.c` given to `compile.sh` takes the same path.

## E. Closed, with the lesson

- **Out-of-line `PERFORM` swallowed the enclosing `END-PERFORM`**
  (sweep, Stage 13): the paragraph form must not `accept` a scope
  terminator that belongs to an outer inline PERFORM.
- **Alphanumeric → numeric MOVE parses the text as decimal**, measured
  against GnuCOBOL (usescreen printed 42.25 for 50.00 before).
- **Report Writer page rules** were measured, not read: the fit test
  counts printing lines; a body line past `LAST DETAIL` spills to a
  new page at `FIRST DETAIL`, with the heading rendered inline by the
  compiler; `TERMINATE` only pads. An earlier "TERMINATE starts a
  page" rule was wrong and is gone.
- **`has_odo` looks at children only** — one occurrence of the ODO
  item itself still moves.
- **Refmod vs subscript**: `x(1:3)` and `x(1)` share the paren; look
  ahead for `:` before parsing a subscript list.
- **Static buffer in `link_name()`** clobbered the main wrapper's
  entry name; copy into a local.
- **2x file statuses are the invalid-key condition**, not errors
  (`file_result` returns 1, the statement's `INVALID KEY` branch runs).
- **tapemgr dropped `binary`/`codepage` on extract** — a majesty bug
  the V round trip found, fixed there (249292b). None found in
  cobc370 yet; when one is, it is filed in `~/cobc370`.
