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
| READ PREVIOUS (2002; 2023 GR 21) | after OPEN at end; after START the record found; after a READ the one before; relative: the first existing lower number | **implemented** (ISSUES-116): 2002/readprev (indexed, the oracle agrees), 2002/readprev2 (after OPEN, relative -- no oracle: GnuCOBOL gives 46 after OPEN and skips or stops wrongly on relative files). Not for LINE SEQUENTIAL (rule 7) or random access (rule 6) |
| READ PREVIOUS of a sequential file (2002; 2023 14.9.30, the file position indicator) | the record before the current one; after START FIRST/LAST the record START found; past the beginning, AT END (10), then 46 until a START or OPEN, as past the end | **implemented** (queue item 17): 2002/startseq (no oracle: GnuCOBOL has no START of a sequential file). Fixed-length records only; variable-length records **refused** by ruling -- the record before has no fixed place to step back to (bad/std2002-readprev-varying). The input block buffer is given up at the first READ PREVIOUS or START; 46 past the end is the indexed and relative files' convention, and GnuCOBOL's for indexed files |
| WRITE (85 sequential 7; 2023 18) | not both ADVANCING PAGE and END-OF-PAGE | **refused**: bad/io-rules -- accepted before |
| WRITE (2023 format; SR 17) | BEFORE and AFTER ADVANCING together, not with PAGE: AFTER n lines before the record, BEFORE m after it | **implemented** under -std=2023 (queue item 31, 2026-10-07): 2023/stmts2023 (a print file, by `cob_write_also_before`; a LINAGE file, whose write takes both counts). Found on the way: on a LINAGE file a WRITE BEFORE n followed by a WRITE AFTER m lost one line of movement, the record's own newline counted for both -- fixed, the file's `pr_state` carrying that the last WRITE's BEFORE took it. **Refused**: bad/std2023-write-both-page, bad/std2014-write-before-after; the counts are literals here |
| WRITE (85 sequential 8; 2023 19) | END-OF-PAGE only with LINAGE | **refused** with the rule -- was a parse error |
| WRITE (2023 3) | no ADVANCING on an indexed or relative file | **refused** -- AFTER ADVANCING 1 slipped through before, the check reading the newline count (zero) rather than the phrase |
| WRITE, REWRITE | INVALID KEY only for indexed and relative files | **refused** |
| WRITE, REWRITE (2023 format 2, the FILE phrase; rules 1, 7) | WRITE FILE file-name FROM, REWRITE FILE file-name FROM: the file's record area | **test**: 2002/fdnorec (implemented 2026-10-06, standard-queue item 11); **refused** without FROM |
| REWRITE (85 relative 3; 2023 2) | no INVALID KEY for a relative file in sequential access | **refused**: bad/io-rules -- accepted before |
| DELETE (85 1; 2023 1-2) | not for a sequential file; no INVALID KEY in sequential access | the first **refused** before; the second now (bad/io-rules) |
| DELETE FILE (2023 14.9.10 format 2; SR 3-4, GR 12-20) | the files removed from storage, each in turn; not open (41); 00, or 05 when not there; 37 when the medium or the authority refuses; OVERRIDE skips the attribute check; ON EXCEPTION | **implemented** under -std=2023 (queue item 31, 2026-10-07; `cob_delete_file`: the path by the ASSIGN, an indexed file's key file removed with it): 2023/stmts2023; **ruling** on GR 19: no fixed file attribute is validated, so 39 never arises; **refused**: a sort file (rule 3), several files inside an exception-checking PERFORM (rule 4), the statement under -std=2014 (bad/std2014-delete-file) |
| START (2023 1-8) | sequential or dynamic access; the key a record key, an item beginning where one does, or the RELATIVE KEY; no NOT = | **refused** (all held) |
| START FIRST, LAST (2002; 2023 14.9.41 GR 11-12, 18-21) | the first or last record: of a relative file by number, existing records only; of an indexed file by the prime key, which becomes the key of reference; of a sequential file by position; none, 23 | **implemented** (queue item 17): 2002/startfirst (indexed and relative; GnuCOBOL agrees on the indexed file, its relative READ PREVIOUS over holes does not -- docs/oracles.md), 2002/startseq (sequential, fixed and variable-length records). Under -std=85 refused as 2002's (bad/std85-start-first) |
| START of a sequential file (2023 14.9.41.3 rule 2) | FIRST or LAST only -- it has no key | **refused** without them (bad/std2002-start-seq-nokey); a LINE SEQUENTIAL file not at all, its records having no fixed place (bad/std2002-start-first-lineseq) |
| START WITH LENGTH (2002; 2023 14.9.41 GR 13-14, rule 8) | an arithmetic expression, the number of leading characters of the key compared (a national key's in characters); outside 1 to the key's length, 23 and no positioning; an indexed file's | **implemented** (queue item 17): 2002/startfirst (a literal, an item, an expression, 0 and too long; GnuCOBOL agrees). On a relative file **refused** (bad/std2002-start-length-relative); on a leading part of the key (an item that begins where it begins) refused -- the item is already the length |

CCVS-85, the Open Systems suite and majesty trip none of the refusals.
