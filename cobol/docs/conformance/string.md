# INSPECT, STRING, UNSTRING: 14.9.22, 14.9.43, 14.9.48

Swept 2026-09-29 (ISSUES-102). X3.23-1985: the Nucleus pages for
INSPECT (VI-89..), STRING (6.25, VI-131..) and UNSTRING (6.27, VI-136..).
2023: 14.9.22, 14.9.43, 14.9.48. CCVS-85 exercises all three (the NC
programs), so the probes went after what CCVS does not test: what must
be refused, and the general rules at their edges. The national and
boolean operand rules were swept earlier (national-boolean.md) and are
only referenced here.

Each refusal cites the edition being compiled: the X3.23-1985 rule
under -std=85, the 2023 rule otherwise.

## INSPECT (14.9.22.3)

| rule | paraphrase | disposition |
|---|---|---|
| 1 | the inspected item is a group or an elementary item of usage display or national | **refused**: bad/inspect-operands -- a COMP item was accepted before this sweep. A function's value (a function-identifier is an identifier, 8.4.3.2) is inspected by TALLYING, which only reads it: **test** 2002/inspfunc (identical to GnuCOBOL); REPLACING and CONVERTING are **refused**, as it is no receiving operand (8.4.3.2.3 rule 1): bad/std2002-inspfunc-replacing, -converting, -numeric. It was "'function' is not declared" before (ISSUES-120) |
| 2 | every other identifier is an elementary item of usage display or national | **refused**: bad/inspect-operands (a COMP item, a group); both accepted before |
| 3 | literals: no ALL figurative constant; nonnumeric | **refused**: bad/inspect-operands (ALL "ab", 12); both accepted before |
| 4, 6 | national all-or-none; national/boolean categories | **refused**: bad/std2002-nat-inspect, bad/std2002-inspect-natop, bad/std2002-bit-inspect |
| 5 | the tally is an elementary numeric item | **test**: any numeric item now; it had to be an integer before (too strict: 9V9 is allowed) |
| 7 (85: 8) | CHARACTERS: the BY operand is one character | **refused**: bad/inspect-operands |
| CONVERTING | the two operands the same size; a repeated FROM character's first occurrence wins | **test**: free/inspectrules (conv) |

General rules (14.9.22.4), all **test**: free/inspectrules, the oracle
agrees on every line -- one pass left to right with the first phrase
that matches taking the positions (overlapping patterns, competing
phrases, their order); LEADING; FIRST; BEFORE and AFTER together;
CHARACTERS in a range and mixed with other phrases; CONVERTING after a
delimiter; TALLYING then REPLACING in one statement.

## STRING (14.9.43.3)

| rule | paraphrase | disposition |
|---|---|---|
| 1 (85: 2) | literals nonnumeric; identifiers but the POINTER of usage display or national | **refused**: bad/string-operands (12, a COMP item); both accepted before this sweep |
| 2 (85: 1) | no ALL figurative constant | **refused**: bad/string-operands (as a source and as a delimiter); accepted before |
| 3 | no zero-length delimiter literal | **refused**: bad/std2014-zero-delimiter; a zero-length source is ignored and a zero-length delimiter item is SIZE (2014/zerolen; refmod.md "8.5.4 Zero-length items") |
| 4 (85: 3) | the receiver not reference-modified | **refused**: bad/string-refmod-receiver |
| 5 (85: 4) | the receiver not edited, not JUSTIFIED | **refused**: "the STRING receiver must be an alphanumeric item, not edited or JUSTIFIED" |
| 6 | not a strongly-typed group | **refused**: "a strongly-typed group is not a STRING receiver" |
| 7 (85: 5) | the POINTER an elementary numeric integer without P, able to hold one more than the receiver's length | **refused**: bad/string-operands (PIC 9 against a 20-character receiver); the size was not checked before, and POINTER without WITH skipped the integer check |
| 8 (85: 6) | a numeric sending item an integer without P | **refused**: bad/string-operands (9V9); accepted before |
| 9 | DELIMITED BY omitted only before INTO | **extension**: omitted anywhere means SIZE, as GnuCOBOL takes it (dialect.md; taskdt) |
| 10 | variable-length groups | **n/a**: dynamic-length items are 2014's |

## UNSTRING (14.9.48.3)

| rule | paraphrase | disposition |
|---|---|---|
| 1 (85: 1) | delimiter literals nonnumeric, no ALL figurative, not zero-length | **refused**: bad/unstring-operands (5), bad/std2014-zero-unstring; ALL is the phrase keyword, not a figurative, here. A zero-length sender ends the statement and a zero-length delimiter item is ignored (2014/zerolen) |
| 2 (85: 2) | the sending item, identifier delimiters and DELIMITER IN items of category alphanumeric or national | **refused**: bad/unstring-operands (a numeric sender, delimiter, DELIMITER IN item); only a non-display numeric sender was refused before. A group or a reference-modified item is alphanumeric |
| 3 | national all-or-none | **refused** (the national sweep): bad/std2002-nat-unstring-num |
| 4 (85: 3) | a receiver: display and alphabetic, alphanumeric or numeric, or national and national or numeric; no P | **refused**: bad/unstring-operands (numeric-edited, 9PP); COMP, edited and P receivers were all accepted before |
| 5 (85: 4) | COUNT IN and TALLYING IN integers without P | **refused**: "COUNT IN needs an integer item" |
| 6 (85: 5) | the POINTER as STRING's, against the sending item's length | **refused**: bad/unstring-operands; not checked before |
| 7 (85: 6) | DELIMITER IN and COUNT IN only with DELIMITED BY | **refused**: "DELIMITER IN without DELIMITED BY" |
| 85: 7 | the sending item not reference-modified | **refused** under -std=85 only (bad/unstring-refmod-85); 2023 dropped the rule |
| 10 | variable-length groups | **n/a**, as STRING |

## General rules (14.9.43.4, 14.9.48.4)

**test**: free/stringrules, the oracle agrees on every line. STRING: a
POINTER of 0 is the overflow, nothing moves and the POINTER keeps its
value; a POINTER inside the receiver, the transfer stopping at its end
with the overflow and the POINTER one past it; only the positions
written change; a multi-character delimiter, a figurative one, a
numeric sender. UNSTRING: DELIMITED BY ALL with COUNT IN and TALLYING
IN; of two delimiters matching at one position the first listed wins
(both orders); numeric, 9V9 and JUSTIFIED receivers; POINTER and
TALLYING with receivers left over (the overflow); a POINTER of 0;
DELIMITER IN under ALL receives one occurrence; more receivers than
fields (no overflow, the rest untouched).

## Found by this sweep

INSPECT, STRING and UNSTRING each accepted operands the text forbids
(COMP items, numeric literals, ALL figuratives, edited or P receivers,
numeric senders and delimiters), and neither statement checked the
POINTER's size. One general rule was wrong: a POINTER of 0 was taken as
"no POINTER" and the statement ran from position 1; the runtime now
receives 1 when there is no POINTER, so 0 is the overflow it should be
(and a national POINTER out of range keeps its value instead of being
set to 1). CCVS-85, the Open Systems suite and majesty are unaffected.
