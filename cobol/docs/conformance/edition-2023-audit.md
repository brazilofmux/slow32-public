# The 2023 edition's substantive changes: 2023 Annex E.2 items 2-30

Audited 2026-10-07 (docs/plans/standard-queue.md item 36). The 2023
edition's Annex E.2 lists the changes from 2014 that can affect an
existing program; one row per item says what this compiler does under
`-std=2014` and under `-std=2023`, and which rows needed a change. Items
1 and 21 (the removals) are edition-2023.md's. Test 2023/e2audit (no
oracle: GnuCOBOL 4 has no -std=cobol2023); bad tests as named.

Three rows changed something: items 5, 13 and 25 are word lists the
compiler keeps per edition. The behaviour rows -- 11, 15-19, 22, 23, 30
-- were probed and hold already; 19 is a ruling that applies to every
edition.

| item | the change | -std=2014 | -std=2023 |
|---|---|---|---|
| 2 | ALIGN among the clauses that typed items must agree on; corresponding bit items aligned alike | **n/a**: TYPE conformance between strongly typed items is by the type's own description (docs/typedef.md), which carries ALIGNED with it, so two items of one type agree by construction | the same |
| 3 | the boolean shifts B-SHIFT-L, -R, -LC, -RC | implemented (docs/boolean.md) under 2002 on, where they were an extension | the same; the words reserved (item 25) |
| 4 | U+037A no longer a user-word character; U+30FB not first or last | **n/a**: user-defined words are ASCII letters, digits, hyphens (and the underscore, BP-E13) here | the same |
| 5 | the compiler-directive words COBOL-WORDS, DISPLAY, FLAG-14, I-O-STATUS-04, NUM-ED-ZERO-FIG-CONSTANT, POP, PUSH, REF-MOD-ZERO-LENGTH, UPON: no compilation-variable names | a >>DEFINE of one of these names was taken | **changed**: **refused** under -std=2023 (copy.h `dirw2023`): bad/std2023-define-dirword |
| 6 | the mode of compile-time arithmetic is the implementor's | long double, the result truncated to an integer where a >>DEFINE takes an expression (directives.md) | the same; FLAG-14 COMPILE-TIME-ARITHMETIC-EXPRESSIONS flags a division |
| 7 | the leap-year formula no longer cited from ISO 8601 | the Gregorian rule (divisible by 4, not by 100 unless by 400): the same formula | the same |
| 8 | the >>EVALUATE end rules: text omitted only when no >>WHEN was true and no >>WHEN OTHER was met | holds: a false >>WHEN omits its text, a >>WHEN OTHER takes it when none was true, nothing after END-EVALUATE is omitted (directives.md) | the same |
| 9 | EC-EXTERNAL conformance conditions | **n/a** | **implemented**: edition-2023.md, item 35 |
| 10 | CONSTANT RECORD with EXTERNAL only strongly typed | **refused**: bad/std2014-constrec-external (item 25) | the same |
| 11 | ALL literal where the context gives no length: the literal's length (8.3.3.6.4 rule 3c) | holds: DISPLAY ALL "ab" shows ab, SPACE and ZERO one character | the same. Test 2023/e2audit |
| 12 | an external file's FILE STATUS the same external item in every program | **n/a** | **implemented**: item 35 (EC-EXTERNAL-DATA-MISMATCH) |
| 13 | FUNCTION ALL INTRINSIC reserves the 2023 function names as user words | the 2002 and 2014 names reserved (edition-2014.md item 13) | **changed**: BASECONVERT, CONCAT, CONVERT, FIND-STRING, MODULE-NAME, SMALLEST-ALGEBRAIC, SUBSTITUTE too: bad/std2023-all-intrinsic-name |
| 14 | two case mappings deleted (U+0131, U+03C2 lowercase) | **n/a**: UPPER-CASE and LOWER-CASE map ASCII letters here | the same |
| 15 | I-O status 04 clarified: a record's length outside the FD's | set on a READ of a record longer or shorter than the FD allows, and a WRITE truncated (libcob `"04"`) | the same |
| 16 | I-O status 07 on OPEN and CLOSE only | set by CLOSE NO REWIND / REEL / UNIT only (item 29); no other statement gives it | the same |
| 17 | whether 0x statuses compare letters without case: the implementor's | **n/a**: every status here is two digits | the same |
| 18 | OPEN may give 37 for insufficient authority | holds: EACCES, EPERM, EROFS, EISDIR on OPEN are 37 | the same |
| 19a | an invalid key condition with no INVALID KEY phrase runs a declarative for the open mode | **ruling**: the 2023 behaviour under every edition -- the statement's own condition goes to the file's USE, then the open mode's (control.h `emit_use_dispatch`; io-statements.md). 2014's rule left the mode form out; GnuCOBOL runs it too | the same. Test 2023/e2audit |
| 19b | a READ exception other than at end or invalid key runs a declarative for INPUT or I-O | holds under every edition: an error status with a USE for the file or the mode runs it | the same |
| 20 | MERGE not in another MERGE's output procedure nor a SORT's procedures | **ruling**: **refused** under every edition (sort.md; `sort_proc_check`), the 1985 SORT rule 1 reading | the same |
| 22 | READ PREVIOUS right after OPEN: the at end condition | holds since ISSUES-116 under every edition | the same |
| 23 | a zero-length reference modification under >>REF-MOD-ZERO-LENGTH, else EC-BOUND-REF-MOD | implemented under -std=2014 (item 24; refmod.md) | the same |
| 24 | an external file's RELATIVE KEY the same external item | **n/a** | **implemented**: item 35 |
| 25 | the reserved words B-SHIFT-L, -LC, -R, -RC, COMMIT, EDITING, END-RECEIVE, END-SEND, EXCLUSIVE-OR, FINALLY, LOCATION, MESSAGE-TAG, RECEIVE, ROLLBACK, SEND, XOR | user-defined words still (a data item named LOCATION compiles) | **changed**: **refused** as names under -std=2023 (diag.h `rw2023`): bad/std2023-reserved-word |
| 26 | transfer-of-control rules include sections | **gap**: the flow rules (14.6.3) are not checked beyond EC-FLOW's conditions (exceptions.md); a PERFORM range's exit is run-time checked the same for a section as for a paragraph | the same |
| 27 | an alphanumeric or national VALUE of a numeric-edited item conforms to the picture | **refused** as before (the literal written edited, 2002 rule 8) | **implemented**: item 32 (value.md rule 7) |
| 28 | VALUE ZERO of a numeric-edited item: the numeric zero, edited | a string of zeros | **implemented**: item 32 |
| 29 | editing symbols required in a literal VALUE, supplied for a numeric one | the literal written edited; a numeric one refused | **implemented**: item 32 |
| 30 | the END-OF-PAGE condition without the phrase: control to the end of the WRITE | holds: the WRITE completes, nothing raised unless EC-I-O-EOP is checked; FLAG-14 WRITE-END-OF-PAGE flags the WRITE | the same. Test 2023/e2audit |
