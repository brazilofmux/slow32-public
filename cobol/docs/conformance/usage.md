# 13.18.60 USAGE clause -- the rules other than BIT and NATIONAL

Swept 2026-09-29 (ISSUES-96). X3.23-1985: VI-47 (USAGE: syntax rules
1-7, general rules). The BIT and NATIONAL rules (5, 7, 12, 13, 17, 20)
were swept with the national and boolean data
([national-boolean.md](national-boolean.md)).

## Syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | no USAGE on a level 66 or 88 entry | **refused**: bad/usage-88 (was "expected a literal in the VALUE") |
| 2 | a subordinate's USAGE is its group's | **refused**: "USAGE of 'a' contradicts the USAGE of its group" |
| 3 | BINARY, COMP and PACKED-DECIMAL take a numeric picture, at any level they are written | **refused**: the message now cites the rule and names PACKED-DECIMAL, not only COMP-3 |
| 4 | no INDEX or pointer in a CONSTANT RECORD | **n/a**: CONSTANT RECORD is 2014's |
| 6 | COMP is COMPUTATIONAL | **test**: throughout |
| 8, 9 | a pointer is referenced only in CALL, INITIALIZE, SET, a relation condition, a function argument or a procedure division header | **refused**: bad/std2002-pointer-ref-display -- DISPLAY of a pointer was accepted before this sweep. ALLOCATE and FREE are not implemented |
| 10 | an index data item is referenced only in SEARCH, SET, a relation condition, a function argument or a USING phrase (85 USAGE syntax rule 5) | **refused**: bad/index-ref-display, index-ref-arith -- DISPLAY, ADD and COMPUTE of one were accepted. MOVE refuses it with its own message (bad/move-index). EVALUATE is allowed, its subjects being compared as a relation is. Index-names (INDEXED BY) are not index data items and are not affected |
| 11 | an index, pointer (or object, message-tag) item is no conditional variable (85 USAGE rule 7) | **refused**: bad/index-88, bad/std2002-pointer-88 -- accepted before this sweep |
| 14 | a pointer at level 1, or in a strongly-typed group (2002 rule 13) | **refused**: bad/std2002-pointer-level -- accepted before |
| 15, 16, 21 | OBJECT REFERENCE, ACTIVE-CLASS, MESSAGE-TAG | **n/a**: object orientation and the message facility are out of scope by ruling |
| 18, 19 | type-name and program-prototype usages need TYPEDEF | **n/a**: those usages are not taken |
| 85 rule 6 | no BLANK WHEN ZERO, JUSTIFIED, PICTURE, SYNCHRONIZED or VALUE on an index item (2023: VALUE 13.16.3 rule 10, JUSTIFIED 13.18.32.3 rule 3, BLANK WHEN ZERO 13.18.8.3 rule 1 -- SYNCHRONIZED is allowed) | **refused**: bad/index-value, index-sync-85, std2002-pointer-value -- all but PICTURE were accepted before this sweep; pointers take the same rules |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | a group's USAGE applies to its elementary items | **test**: 2002/natnum, CCVS NC |
| 4, 6 | BINARY and COMPUTATIONAL: the implementor's radix-2 form | **ruling**: 2, 4 or 8 bytes by digits; COMP-5 1, 2, 4 or 8 (docs/dialect.md, "COMP/BINARY width") |
| 7 | DISPLAY: the alphanumeric character set | **ruling**: UTF-8 (docs/national.md, the encoding rulings) |
| 10 | INDEX: an occurrence number's representation | **ruling**: four bytes, as a pointer is |
| 11 | PACKED-DECIMAL: a digit a nibble; WITH NO SIGN | **test**: free/identmove, CCVS NC; NO SIGN is 2014's: **n/a** |
| 12 | BINARY-CHAR, -SHORT, -LONG, -DOUBLE, SIGNED by default, hold their minimum ranges | **test**: 2002/binranges (the oracle agrees). SIGNED and UNSIGNED after BINARY-SHORT and BINARY-LONG were not parsed before this sweep ("unexpected 'unsigned'"). BINARY-DOUBLE is **gap**: its range needs 19 digits, the arithmetic here holds 18; refused by name (bad/std2002-binary-double) |
| 13-18 | the floating-point usages | **n/a** until -std=2014 (the ruling of 2026-09-28); refused as not implemented |
| POINTER | a pointer holds a data address, NULL the null one; INITIALIZE sets NULL | **test**: 2002/pointerset (the oracle agrees). `SET p TO NULL` failed ("null cannot be moved to the pointer item") and `p = NULL` was never true, NULL being compared as four alphanumeric bytes; both fixed |
| ADDRESS OF, BASED | 2002 8.4.2.11 / 2023 8.4.3.11 (rules 2, 4, 5), BASED 2002 13.16.5, SET formats 7 and 10, pointer relations (8.8.4.2.2 format 3, 8.8.4.2.3 rule 5, 8.8.4.2.16) | **test**: 2002/addressof (the oracle agrees), 2002/ecptrnull (EC-DATA-PTR-NULL, 13.16.5 general rule 3); **refused**: bad/std2002-address-of-display (rule 5), -set-address-ws (14.9.39.3 rule 18), -pointer-lt, -pointer-cmp-num, address-of-85. Implemented after this sweep named it a gap (ISSUES-96). **gap**: ADDRESS OF passed BY REFERENCE or BY CONTENT (bad/std2002-address-of-byref), ALLOCATE and FREE, BASED in LOCAL-STORAGE; EC-BOUND-PTR (13.16.5 general rule 4) is not raised |

## Rulings recorded

- `SET ADDRESS OF` a LINKAGE record at level 01 or 77 that is not
  BASED: accepted, as IBM and GnuCOBOL accept it and as older programs
  written for them do. 2023 14.9.39.3 rule 18 asks for a based item; a
  LINKAGE record is reached through the same kind of cell here.

- Under -std=85 the compiler takes, as extensions majesty uses,
  COMP-3, COMP-5, BINARY-CHAR, SIGNED-INT, SIGNED-SHORT,
  UNSIGNED-SHORT and POINTER (docs/dialect.md). The rules above apply
  to them as in 2002.
- COMP-1 is RM/COBOL's: a binary integer with a PICTURE, as every
  COMP-1 item in the Open Systems suite has. IBM's hexadecimal float
  COMP-1/COMP-2 stays out (the ruling of 2026-09-28).

## Found by this sweep

Rules 8-11 and 14, and 85's rule 6, were not enforced. BINARY-SHORT
and BINARY-LONG took no SIGNED or UNSIGNED phrase. SET of a pointer to
NULL did not compile, and no pointer compared equal to NULL. ADDRESS
OF was misread as a qualified name ("'address' is not declared under
'w'"). CCVS-85, the Open Systems suite (229 programs byte-identical
under -std=85) and majesty are unaffected.
