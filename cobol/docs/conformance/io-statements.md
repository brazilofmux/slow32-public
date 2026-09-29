# The I-O statements: OPEN, CLOSE, READ, WRITE, REWRITE, DELETE, START

Swept 2026-09-29 (ISSUES-111). X3.23-1985: the Sequential, Relative and
Indexed I-O modules' statements. 2023: 14.9.6, .10, .27, .30, .35,
.41, .51. The general rules (I-O status, positioning, the AT END and
INVALID KEY conditions) are exercised at length by CCVS-85's SQ, RL
and IX programs, which match GnuCOBOL; this sweep went after the syntax
rules.

| rule | paraphrase | disposition |
|---|---|---|
| OPEN (85 sequential 3, relative/indexed 1; 2023 2) | EXTEND only in sequential access and without LINAGE | **refused**: bad/io-rules -- accepted before this sweep |
| OPEN (85 formats; 2023 5-6) | NO REWIND only for a sequential file opened INPUT or OUTPUT | **refused**: bad/io-rules -- accepted before |
| OPEN (2023 1; 85 Report Writer format) | a report file not opened INPUT or I-O | **refused**: bad/open-report-input -- accepted before |
| CLOSE (85 relative/indexed format; 2023 1) | REEL, UNIT, NO REWIND only for sequential files | **refused**: bad/io-rules -- accepted before |
| READ 85 rule 1 | INTO is not the file's own record area | **refused**: bad/io-rules -- accepted before |
| READ 2023 rule 1 | several record descriptions: INTO and every record alphanumeric | **refused** under -std=2002 -- accepted before |
| READ 85 rule 2, WRITE/REWRITE/DELETE/START likewise | AT END or INVALID KEY required when no USE procedure applies | **extension** BP-E18: the condition goes to the FILE STATUS or stops the run; the Open Systems suite has 8 |
| READ 2023 6, 10-11 | no AT END or NEXT in random access; KEY only for an indexed file, naming one of its keys | **refused** |
| READ PREVIOUS (2002) | | **not implemented**, said so (it was a parse error) |
| WRITE (85 sequential 7; 2023 18) | not both ADVANCING PAGE and END-OF-PAGE | **refused**: bad/io-rules -- accepted before |
| WRITE (85 sequential 8; 2023 19) | END-OF-PAGE only with LINAGE | **refused** with the rule -- was a parse error |
| WRITE (2023 3) | no ADVANCING on an indexed or relative file | **refused** -- AFTER ADVANCING 1 slipped through before, the check reading the newline count (zero) rather than the phrase |
| WRITE, REWRITE | INVALID KEY only for indexed and relative files | **refused** |
| REWRITE (85 relative 3; 2023 2) | no INVALID KEY for a relative file in sequential access | **refused**: bad/io-rules -- accepted before |
| DELETE (85 1; 2023 1-2) | not for a sequential file; no INVALID KEY in sequential access | the first **refused** before; the second now (bad/io-rules) |
| START (2023 1-8) | sequential or dynamic access; the key a record key, an item beginning where one does, or the RELATIVE KEY; no NOT = | **refused** (all held) |

CCVS-85, the Open Systems suite and majesty trip none of the refusals.
