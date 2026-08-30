# Report Writer

Majesty currently uses GnuCOBOL's Report Writer for the general-ledger
reports. Retiring GnuCOBOL means this compiler's RW has to print those
reports. cobc370 just finished the 1974 module against the standard's
own tables; that work is the teacher, not the code.

## Majesty's subset (v1)

Survey of `~/majesty/src/cobol/*.cbl` that contain `report section`
(`gl008`, `gl015`, `gl016`, `gl022`, `gl023`, `gl030`, `gl036`,
`gl042`, `gl043`):

Used:

- `RD` with `PAGE LIMIT`, `HEADING`, `FIRST DETAIL`, `LAST DETAIL`
- `TYPE PAGE HEADING`, `TYPE DETAIL`
- `LINE n`, `LINE PLUS n`
- `COLUMN n`
- `SOURCE` identifier, `VALUE` literal
- edited pictures on printable items
- `INITIATE` / `GENERATE` / `TERMINATE`
- `LINE-COUNTER` / `PAGE-COUNTER` as implied (if a program names
  them; check per file when implementing)

Not used, anywhere in that tree:

- `CONTROL IS` / `CONTROLS ARE`
- `TYPE CONTROL HEADING` / `FOOTING`
- `TYPE REPORT HEADING` / `FOOTING` / `PAGE FOOTING`
- `SUM … UPON … RESET`
- `GROUP INDICATE`
- `NEXT GROUP`
- `USE BEFORE REPORTING` / `SUPPRESS`
- `CODE`

Totals are Procedure Division `COMP-3` items, then `SOURCE` of those
items. Control breaks are done by hand. That is the cheap half of
Report Writer — the same subset cobc370's original language survey
found in the MVS corpus — and it is enough to retire GnuCOBOL for
these reports.

`gl030` is the v1 gate: page heading plus several detail groups
(`transaction-line`, `description-line`, `debit-line`, `credit-line`,
`total-line`), `LINE PLUS`, edited amounts.

Three details from reading the gates rather than surveying them:

- gl022 writes the first line of its page heading as `line +1` —
  the signed form, no `PLUS` word — and gl030 writes `line plus 2`
  on a detail. Both spellings, and `LINE n` absolute, are v1.
- The amount picture is `----,---,--9.99`, printed at columns 65 and
  81. It is the exact picture on which cobc370's floating minus was
  found misplaced for small negatives (`DIFFERENTIAL-TESTING.md`,
  GL035/GL036), and the journal contains only positive amounts, so
  matching `journal-*.prn` proves nothing about the sign. Put a
  negative through it in a unit test.
- Dates print `pic 9(5) source year` — a **five-digit year**, by
  design (`data-formats.md`); `year` is a `signed-short` in the
  fielded-date group a user function returned.

## How to compile it

cobc370's approach, which survived contact with the 1974 tables:

- A per-report state block: LINE-COUNTER, PAGE-COUNTER, whether a
  body group has been presented on this page, whether the first
  `GENERATE` has happened, a physical-line cell if NEXT GROUP later
  arrives (it diverges from LINE-COUNTER the moment a group moves
  the register without moving paper).
- Per-group presentation code generated from the standard's tables,
  indexed by things known at compile time (`TYPE`, LINE sequence
  shape).
- `SOURCE`/`VALUE`/`COLUMN` render by positioning fields on a line
  buffer, then writing the line. That is PICTURE + MOVE, not a
  new numeric system.
- Fit tests from the standard, not from GnuCOBOL. cobc370 found
  GnuCOBOL's RW unfit as an oracle for page-fit, `NEXT GROUP`, and
  several `LINE-COUNTER` rules. For majesty v1, GnuCOBOL *output* is
  the oracle (byte-identical `.prn`); if a fit-test disagreement
  appears, the 85 text decides and the oracle is corrected by hand.

The 85 Report Writer is close to 74. Do not share cobc370's parser
anyway: free-format, ASCII, no `ED` instruction, different file
writer (line sequential print files, not QSAM ASA). Share the
*table-driven renderer* idea.

Print files in majesty are `ORGANIZATION IS LINE SEQUENTIAL` with
`ASSIGN` to a data-name (`ws-output-filename`). Carriage control is
newlines, not ASA. `WRITE … ADVANCING` on non-report sequential
files is a separate, later item.

## As built (Stage 7)

The page model, measured on majesty's `.prn` files and then
reproduced byte for byte:

- A page is exactly `PAGE LIMIT` physical lines. Every physical line
  is one line-sequential record (trailing spaces removed); the lines
  a `LINE` clause skips, and the tail of every page including the
  last, are empty records. There is no form feed.
- The page heading is presented when the first body group of a page
  is generated; an absolute `LINE n` lands on line n, a relative one
  on LINE-COUNTER + n (LINE-COUNTER is 0 at the top of a page, so
  gl022's `line +1, +1, +2` gives lines 1, 2 and 4).
- The first body group on a page with a relative first line lands on
  `FIRST DETAIL` -- the 85 rule -- so gl030's `line plus 2`
  transaction lands on line 6 after a three-line heading, not 5 or 8.
  Later groups land on LINE-COUNTER + n.
- Fit: a body group whose last line (first line plus the relative
  extent of its remaining lines) would pass `LAST DETAIL` advances the
  page first: blank lines to `PAGE LIMIT`, then the heading again.
- `TERMINATE` pads the current page to `PAGE LIMIT`; with nothing
  generated it prints nothing.
- Two rules measured later on the profit-and-loss report and
  the activity report (Stage 15): the fit test counts a group's
  *printing* lines -- a trailing `LINE PLUS 1` with no fields does not
  keep a group off the page (gl043's "Net Profit" group, one printing
  line and one empty, lands on line 60 of 60); and a body line that
  would land past `LAST DETAIL` **spills onto a new page**: the heading
  is presented and the line lands on `FIRST DETAIL`, the rest of the
  group following it. That is why that profit-and-loss report is two pages, the
  second a heading over one blank line and padding, and why an
  activity page can open with the blank tail of the group that closed
  the page before. `TERMINATE` itself only pads.
- Fields render by `MOVE` into a 512-column line buffer -- the
  ordinary conversion matrix, editing included -- so
  `----,---,--9.99` with a negative amount prints the sign against
  the digits (`tests/fixed/report.cbl` puts -3,765.44 through it).

The state block per report (`cob_report`) carries the file, the four
RD numbers, LINE-COUNTER, PAGE-COUNTER and the body-seen flag; the
compiler emits the fit test and the heading inline at each
`GENERATE` site.

## After v1

`CONTROL` / `CH`/`CF` / `SUM` / `USE BEFORE REPORTING` are the
expensive half. cobc370 implemented them because the MVS corpus and
the 1974 claim required it. Majesty does not require them today.
Implement when a program asks, against the 85 text, with GnuCOBOL
consulted only where it agrees. cobc370's Report Writer slices in
`COBOL74-ROADMAP.md` are the work-order if that day comes; the
oracles will need to be rebuilt for 85 where 74 and 85 diverge.
