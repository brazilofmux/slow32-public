# Report Writer: X3.23-1985 XIII; 2023 13.8, 13.14, 13.15, the report clauses, 14.9.16, 14.9.21, 14.9.45, 14.9.46

Swept 2026-09-30, against the syntax rules of the 1985 module. The 2023
text (13.18 and 14.9 for GENERATE, INITIATE, SUPPRESS and TERMINATE)
keeps them and adds clauses this compiler does not have, noted at the
end. How the module is built, and where GnuCOBOL's output differs from
the text, is in docs/report-writer.md.

CCVS-85's RW module (RW101A-RW104A, RW301M, RW302M) tests what must be
accepted. All six compile and pass after the rules below were enforced,
each matching GnuCOBOL's tally, and majesty's reports are unchanged.
Until this sweep almost none of the rules were checked. The probes that
found that are now thirty refusal tests, `tests/bad/rw85-*`.

## RD entry (3.5-3.8; 2023 13.8 the report section, 13.14 the report description entry, 13.18.12 CODE, 13.18.16 CONTROL, 13.18.39 PAGE, 13.18.46 REPORT)

| rule | paraphrase | disposition |
|---|---|---|
| 3.5 SR 1; 3.3 SR 1-2 | a report-name in one REPORT clause, and an RD for each | "no FD says REPORT IS" |
| 3.6 SR 1-2 | CODE: a two-character literal, on every report of the file or none | **refused**: rw85-code-not-two, bad/rw-code-partial |
| 3.7 SR 1 | a CONTROL data-name is not a Report Section item | refused at its lookup ("is not declared") |
| 3.7 SR 2 | each CONTROL data-name a different item | **refused**: rw85-control-duplicate |
| 3.8 SR 2 | PAGE LIMIT at most three significant digits | **refused**: rw85-page-limit-4digits |
| 3.8 SR 3-7 | 1 ≤ HEADING ≤ FIRST DETAIL ≤ LAST DETAIL ≤ FOOTING ≤ PAGE LIMIT | **refused**: rw85-page-heading0, -first-lt-heading, -last-lt-first, -footing-lt-last, -limit-lt-footing |
| 3.8 SR 8 | each group type within its region of the page (2002 and 2023 alike, with the same defaults: FIRST DETAIL omitted is HEADING, so a PAGE HEADING needs a FIRST DETAIL below it; 2002/natreport lacked one) | **refused** for absolute lines, and a group taller than its region: rw85-ph-outside-region, -de-outside-region, -pf-outside-region. The regions are those of the rule: RH from HEADING (to FIRST DETAIL - 1, or the page, on a page by itself), PH from HEADING to FIRST DETAIL - 1, CH and DE from FIRST DETAIL to LAST DETAIL, CF from FIRST DETAIL to FOOTING, PF from FOOTING + 1 to PAGE LIMIT, RF after FOOTING (or the page, on a page by itself) |
| 3.8 SR 9 | every group fits on one page | as the region check |

## Report group description entry (3.9; 2023 13.15) and its clauses (13.18.14 COLUMN, 13.18.28 GROUP INDICATE, 13.18.35 LINE, 13.18.37 NEXT GROUP, 13.18.53 SOURCE, 13.18.54 SUM, 13.18.57 TYPE format 2)

| rule | paraphrase | disposition |
|---|---|---|
| 3.9 SR 3 | levels 02-49 | **refused**: "bad level" |
| 3.9 SR 9; 3.15 SR 2 | no LINE in an entry under another with LINE | **refused**: rw85-line-in-line |
| 3.9 SR 10a; 3.13 SR 1 | GROUP INDICATE in a DETAIL group only | **refused**: rw85-group-indicate-cf (it was a parse error, "'group' is not declared", the SUM operands having run on into it) |
| 3.9 SR 10b; 3.19 SR 3 | SUM in a CONTROL FOOTING only | **refused**: "SUM belongs in a CONTROL FOOTING group" |
| 3.9 SR 10c; 3.11 SR 1 | COLUMN in or under an entry with LINE | **refused**: "a printable entry ... before any LINE" |
| 3.9 SR 10e | VALUE needs COLUMN | **refused**: rw85-value-no-column |
| 3.11 SR 2 | the printable items of a line ascend and do not overlap | **refused**: rw85-column-descending, rw85-column-overlap |
| 3.11 GR 1 | no COLUMN: the item is not presented | **test**: free/rwsign. Fixed in this sweep. Such an entry was presented after the item before it. A SUM so defined is a counter only, and still sums. GnuCOBOL presents it at column 1 (docs/oracles.md) |
| 3.15 SR 1 | LINE at most three significant digits, within the group's region | as 3.8 SR 8 |
| 3.15 SR 3-4 | the absolute LINEs of a group come first, ascending | **refused**: rw85-absolute-after-relative, rw85-absolute-descending |
| 3.15 SR 5 | without PAGE, only relative LINEs | **refused**: rw85-no-page-absolute |
| 3.15 SR 6 | NEXT PAGE once, in the group's first LINE | **refused**: "NEXT PAGE is in the first LINE clause". `LINE n ON NEXT PAGE`, the 1985 form, is now accepted; only `LINE NEXT PAGE` was |
| 3.15 SR 7 | LINE ... NEXT PAGE only in body groups and the REPORT FOOTING | **refused**: rw85-next-page-ph |
| 3.15 SR 9 | a PAGE FOOTING's first LINE is absolute | **refused**: rw85-pf-first-relative |
| 3.16 SR 1 | NEXT GROUP only in a group with a LINE | **refused**: "has no LINE" |
| 3.16 SR 3 | without PAGE, only NEXT GROUP PLUS | **refused**: "only NEXT GROUP PLUS" |
| 3.16 SR 4-5 | no NEXT GROUP NEXT PAGE in a PAGE FOOTING; no NEXT GROUP in a PAGE HEADING or REPORT FOOTING | **refused**: rw85-next-group-pf-next-page, rw85-next-group-ph |
| 3.17 SR 1-3 | SIGN: a signed numeric PICTURE, usage display, SEPARATE required | **test**: free/rwsign (LEADING and TRAILING SEPARATE, both agreeing with the oracle); **refused**: rw85-sign-not-separate. SIGN was refused as "unexpected 'sign'" |
| 3.18 SR 1 | a Report Section SOURCE is LINE-COUNTER, PAGE-COUNTER or a sum counter | refused at its lookup: other report entries have no data item |
| 3.19 SR 1 | SUM operands numeric; with UPON, not sum counters; the entry not alphabetic | **refused**: rw85-sum-alnum, and the other two by message |
| 3.19 SR 2 | UPON names a DETAIL group of the report | **refused**: "UPON ... is not a DETAIL group" |
| 3.19 SR 4 | RESET ON a CONTROL data-name no lower than the footing's own; FINAL only with CONTROL FINAL | **refused**: rw85-reset-final-no-final, and "a control no lower than the footing's own" |
| 3.20 SR 2 | RH, PH, CH FINAL, CF FINAL, PF and RF once each | **refused**: rw85-two-ph |
| 3.20 SR 3 | PH and PF only with a PAGE clause | **refused**: rw85-ph-no-page |
| 3.20 SR 4 | CH and CF name a control, one of each a control | **refused**: rw85-two-cf; "is not in RD ...'s CONTROL clause" |
| 3.20 SR 7 | at least one body group | **refused**: "has no body group" |
| 3.21 SR 3 | USAGE DISPLAY (2002: or NATIONAL) | **refused**: bad/std2002-rw-usage |
| 3.22 SR 2 | a VALUE fits its PICTURE | **refused**: rw85-value-too-long |

## Statements (4.2-4.9; 2023 14.9.16 GENERATE, 14.9.21 INITIATE, 14.9.45 SUPPRESS, 14.9.46 TERMINATE)

| rule | paraphrase | disposition |
|---|---|---|
| 4.3 SR 1 | GENERATE data-name names a DETAIL group | **refused**: "GENERATE needs a DETAIL group" |
| 4.3 SR 2 | GENERATE report-name: a CONTROL clause, at most one DETAIL group | **refused**: rw85-generate-report-two-details, "the RD has no CONTROL clause". The one-detail limit is 1985's: 2002 (14.8.15.2) and 2023 drop it, so it is refused under `-std=85` only. This suite's own rptuse drives summary reporting over two details, and moved to tests/2002 |
| 4.5 SR 1 | OPEN OUTPUT or EXTEND only, for a report file | **refused**: bad/open-report-input |
| 4.6 SR 1 | SUPPRESS only in a USE BEFORE REPORTING procedure | **refused**: "SUPPRESS belongs in a USE BEFORE REPORTING section" |
| 4.9 SR 2 | a report group in one USE BEFORE REPORTING only | **refused**: "two USE BEFORE REPORTING procedures" |
| 4.9 SR 3 | no GENERATE, INITIATE or TERMINATE in a USE BEFORE REPORTING procedure | **refused**: "GENERATE in a USE BEFORE REPORTING procedure" |
| general rules | the RWCS sequence, sums, breaks, pages | **test**: the CCVS RW module, fixed/report, free/rwpage, free/rptctl, free/rptnext, 2002/rptuse, fixed/rwcode, fixed/rwnested (docs/report-writer.md) |

## 2002 and later (13.18.14 format 1, 13.18.35 format 1, 13.18.38 format 3, 13.18.41, 13.18.64)

Implemented 2026-10-07 (docs/plans/standard-queue.md item 37), under
-std=2002 and later. The report group is still compiled to code, line by
line and item by item (helpers.h `emit_report_group`): a PRESENT WHEN
condition is parsed at its recorded position and branched on; OCCURS and
a multiple LINE or COLUMN clause unroll the repetitions, a DEPENDING ON
count tested at run time; VARYING's item is a temporary of the entry's,
set FROM before the first repetition and stepped BY after each; COLUMN
PLUS goes through libcob's horizontal counter (`cob_rw_field_rel`). Test
2002/rw2002 (no oracle: GnuCOBOL 4 has no VARYING in a report, and for
the rest generates C that does not compile -- docs/oracles.md); bad tests
as named. Majesty and the Open Systems papers use the 1985 module.

| rule | paraphrase | disposition |
|---|---|---|
| COLUMN SR 9, GR 6 | LEFT (the default), RIGHT or CENTER: what the number names; not with PLUS | **test**: rw2002 (`RIGHT 20`, `CENTER 30`); **refused**: bad/std2002-rw-column-mode-plus; CENTER's even and odd widths by GR 6d |
| COLUMN GR 7-9 | PLUS n: n beyond the line's horizontal counter, the rightmost column occupied | **test**: rw2002 (`PLUS 3` after an item ending at 31 lands at 34; `PLUS 1` on OCCURS items) |
| COLUMN SR 10 | several numbers in one clause: the item at each, increasing, not with OCCURS | **test**: rw2002 (`COLUMN 40 50 60` with VARYING); **refused**: bad/std2002-rw-multicol-order, -multicol-occurs |
| COLUMN SR 7, 8a; LINE SR 6 | items (lines) overlapping, or out of order, each under a different PRESENT WHEN | the 1985 overlap check now takes the leftmost column LEFT, RIGHT, CENTER or PLUS gives, every number and occurrence, and leaves out an item with a PRESENT WHEN (bad/std2002-rw-overlap-plus: RIGHT 1 is before column 1); lines are not checked against each other |
| LINE SR 10 | a multiple LINE clause: the line at each number or PLUS step, all absolute or all relative | **test**: rw2002 has the OCCURS form; the clause parses (`LINES ARE 5 7 9`, `LINE PLUS 1 PLUS 1`), each repetition a line |
| PRESENT WHEN GR 2 | the entry, its subordinates, or the whole group (on the 01) absent when false | **test**: rw2002 (an item, a line, a CONTROL FOOTING group); **refused**: bad/rw2002-85 under -std=85 |
| PRESENT WHEN GR 3 | the arrangement rules take absent items into account; an absent SUM entry is not printed and not reset | the overlap check (above); a SUM entry's counter is kept whether printed or not: **gap**, the reset is unconditional |
| OCCURS format 3 SR 24-27, GR 10-12 | n [TO m DEPENDING ON item] [STEP s] on a printable item (horizontal) or a LINE entry (vertical); STEP required for absolute positions | **test**: rw2002 (an item `OCCURS 1 TO 5 DEPENDING ON ndeps STEP 4`, a line `OCCURS 2`); **refused**: bad/std2002-rw-occurs-step, -occurs-to-dep; OCCURS on a group entry that is neither a line nor an item (GR 10b, 10d) is **refused** as not implemented; 64 occurrences at most |
| VARYING SR 1-3, GR 1-3 | data-name-1 a temporary integer of the entry's, FROM (1) before the first repetition, BY (1) after each; with OCCURS or a multiple clause only | **test**: rw2002 (`VARYING ix FROM 1 BY 2` over `COLUMN 40 50 60`, `VARYING iy FROM 4` over a line's OCCURS, the DEPENDING ON item's); **refused**: bad/std2002-rw-varying-alone, -varying-dup; one VARYING item per entry here (the clause allows several) |
