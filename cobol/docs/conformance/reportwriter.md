# Report Writer: X3.23-1985 XIII

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

## RD entry (3.5-3.8)

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

## Report group description entry (3.9) and its clauses

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

## Statements (4.2-4.9)

| rule | paraphrase | disposition |
|---|---|---|
| 4.3 SR 1 | GENERATE data-name names a DETAIL group | **refused**: "GENERATE needs a DETAIL group" |
| 4.3 SR 2 | GENERATE report-name: a CONTROL clause, at most one DETAIL group | **refused**: rw85-generate-report-two-details, "the RD has no CONTROL clause". The one-detail limit is 1985's: 2002 (14.8.15.2) and 2023 drop it, so it is refused under `-std=85` only. This suite's own rptuse drives summary reporting over two details, and moved to tests/2002 |
| 4.5 SR 1 | OPEN OUTPUT or EXTEND only, for a report file | **refused**: bad/open-report-input |
| 4.6 SR 1 | SUPPRESS only in a USE BEFORE REPORTING procedure | **refused**: "SUPPRESS belongs in a USE BEFORE REPORTING section" |
| 4.9 SR 2 | a report group in one USE BEFORE REPORTING only | **refused**: "two USE BEFORE REPORTING procedures" |
| 4.9 SR 3 | no GENERATE, INITIATE or TERMINATE in a USE BEFORE REPORTING procedure | **refused**: "GENERATE in a USE BEFORE REPORTING procedure" |
| general rules | the RWCS sequence, sums, breaks, pages | **test**: the CCVS RW module, fixed/report, free/rwpage, free/rptctl, free/rptnext, 2002/rptuse, fixed/rwcode, fixed/rwnested (docs/report-writer.md) |

## 2002 and later

PRESENT WHEN, COLUMN PLUS, LEFT, RIGHT and CENTER, several column
numbers in one clause, VARYING, and OCCURS in report groups are not
implemented. Each is refused by name ("... is COBOL 2002's Report
Writer; not implemented"); until this sweep they met a bare parse error.
Majesty and the Open Systems papers use the 1985 module.
