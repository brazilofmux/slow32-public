# 13.18.60 USAGE clause -- the rules other than BIT and NATIONAL; 8.5.2.15 and 8.4.3.13 program-pointers

Swept 2026-09-29 (ISSUES-96). X3.23-1985: VI-47 (USAGE: syntax rules
1-7, general rules). The BIT and NATIONAL rules (5, 7, 12, 13, 17, 20)
were swept with the national and boolean data
([national-boolean.md](national-boolean.md)). Program-pointers were
added 2026-10-06 (docs/plans/standard-queue.md item 9): the table at
the end.

## Syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | no USAGE on a level 66 or 88 entry | **refused**: bad/usage-88 (was "expected a literal in the VALUE") |
| 2 | a subordinate's USAGE is its group's | **refused**: "USAGE of 'a' contradicts the USAGE of its group" |
| 3 | BINARY, COMP and PACKED-DECIMAL take a numeric picture, at any level they are written | **refused**: the message now cites the rule and names PACKED-DECIMAL, not only COMP-3 |
| 4 | no INDEX or pointer in a CONSTANT RECORD | **refused** (2026-10-07; data-division.md "13.18.15 CONSTANT RECORD") |
| 6 | COMP is COMPUTATIONAL | **test**: throughout |
| 8, 9 | a pointer is referenced only in CALL, INITIALIZE, SET, a relation condition, a function argument or a procedure division header | **refused**: bad/std2002-pointer-ref-display -- DISPLAY of a pointer was accepted before this sweep. ALLOCATE and FREE are not implemented |
| 10 | an index data item is referenced only in SEARCH, SET, a relation condition, a function argument or a USING phrase (85 USAGE syntax rule 5) | **refused**: bad/index-ref-display, index-ref-arith -- DISPLAY, ADD and COMPUTE of one were accepted. MOVE refuses it with its own message (bad/move-index). EVALUATE is allowed, its subjects being compared as a relation is. Index-names (INDEXED BY) are not index data items and are not affected |
| 11 | an index, pointer (or object, message-tag) item is no conditional variable (85 USAGE rule 7) | **refused**: bad/index-88, bad/std2002-pointer-88 -- accepted before this sweep |
| 14 | a pointer at level 1, or in a strongly-typed group (2002 rule 13) | **refused**: bad/std2002-pointer-level -- accepted before |
| 15, 16, 21 | OBJECT REFERENCE, ACTIVE-CLASS, MESSAGE-TAG | **n/a**: object orientation and the message facility are out of scope by ruling |
| 18, 19 | type-name and program-prototype usages need TYPEDEF | **ruling**: read as "the name is one a TYPEDEF (a prototype) declared" -- a PROGRAM-POINTER TO prototype-name is taken on any level-1 item, the prototype being one of the REPOSITORY's (bad/std2002-pptr-to-unknown); POINTER TO type-name is not taken |
| 85 rule 6 | no BLANK WHEN ZERO, JUSTIFIED, PICTURE, SYNCHRONIZED or VALUE on an index item (2023: VALUE 13.16.3 rule 10, JUSTIFIED 13.18.32.3 rule 3, BLANK WHEN ZERO 13.18.8.3 rule 1 -- SYNCHRONIZED is allowed) | **refused**: bad/index-value, index-sync-85, std2002-pointer-value -- all but PICTURE were accepted before this sweep; pointers take the same rules |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | a group's USAGE applies to its elementary items | **test**: 2002/natnum, CCVS NC |
| 4, 6 | BINARY and COMPUTATIONAL: the implementor's radix-2 form, alignment, sign and range | **ruling** (docs/usage.md): two's complement, 2, 4 or 8 bytes by digits, **big-endian** as IBM, Micro Focus and GnuCOBOL store it (`-fbinary-byteorder=native` for SLOW-32's order); COMP-5 and the native usages 1, 2, 4 or 8 bytes in the machine's order. **test**: free/binorder (the bytes of a written record; the oracle agrees) |
| 7 | DISPLAY: the alphanumeric character set | **ruling**: UTF-8 (docs/national.md, the encoding rulings) |
| 10 | INDEX: an occurrence number's representation | **ruling**: four bytes, as a pointer is |
| 11 | PACKED-DECIMAL: a digit a nibble; WITH NO SIGN | **test**: free/identmove, CCVS NC; WITH NO SIGN (2023 GR 25: no sign nibble, the picture without S) **implemented** under -std=2023 (queue item 31, 2026-10-07) as the standard's spelling of COMP-6 (UV_NOSIGN, the digits right-aligned in (n + 1) / 2 bytes): 2023/stmts2023; **refused**: bad/std2023-packed-no-sign-s (an S), bad/std2014-packed-no-sign (2014) |
| 12 | BINARY-CHAR, -SHORT, -LONG, -DOUBLE, SIGNED by default, hold their minimum ranges | **test**: 2002/binranges (the oracle agrees). SIGNED and UNSIGNED after BINARY-SHORT and BINARY-LONG were not parsed before this sweep ("unexpected 'unsigned'"). BINARY-DOUBLE [SIGNED | UNSIGNED]: **test** 2002/bindouble, the oracle agrees (implemented in ISSUES-117 on the 31-digit path; it was refused by name) |
| 13 | FLOAT-SHORT, -LONG, -EXTENDED: the implementor's floating-point formats, each holding what the one before holds | **ruling** (docs/usage.md, 2026-09-30): IEEE single, double, and double again for FLOAT-EXTENDED, in the machine's order. **test**: 2002/floatext (the values; the oracle agrees), free/comp12, 2002/floatsort |
| 14-18 | FLOAT-BINARY-32/64/128, FLOAT-DECIMAL-16/34: ISO/IEC 60559's formats | **implemented** under -std=2014 (queue item 20, 2026-10-07): binary32 and binary64 in the hardware's float and double (as FLOAT-SHORT and -LONG), binary128, decimal64 and decimal128 in software (libcob/ieee.h), read into and written from the wide decimal stack, correctly rounded to nearest-even both ways, the statement computing as floating decimals of 36 digits (docs/usage.md). The endianness and encoding phrases and the OPTIONS defaults (options.md). **Test**: 2014/floatdec (GnuCOBOL agrees on the decimal formats through PICTURE receivers; its own DISPLAY form differs), 2014/floatbin (no oracle). **Refused** under -std=2002 as 2014's: bad/std2002-float-binary; with a PICTURE: bad/std2014-float-picture |
| POINTER | a pointer holds a data address, NULL the null one; INITIALIZE sets NULL | **test**: 2002/pointerset (the oracle agrees). `SET p TO NULL` failed ("null cannot be moved to the pointer item") and `p = NULL` was never true, NULL being compared as four alphanumeric bytes; both fixed |
| ADDRESS OF, BASED | 2002 8.4.2.11 / 2023 8.4.3.11 (rules 2, 4, 5), BASED 2002 13.16.5, SET formats 7 and 10, pointer relations (8.8.4.2.2 format 3, 8.8.4.2.3 rule 5, 8.8.4.2.16) | **test**: 2002/addressof (the oracle agrees), 2002/ecptrnull (EC-DATA-PTR-NULL, 13.16.5 general rule 3); **refused**: bad/std2002-address-of-display (rule 5), -set-address-ws (14.9.39.3 rule 18), -pointer-lt, -pointer-cmp-num, address-of-85. Implemented after this sweep named it a gap (ISSUES-96). ADDRESS OF BY REFERENCE and BY CONTENT pass the unique data item it creates (8.4.3.11 GR 1), a compiler-made pointer: 2002/addressofarg (the oracle agrees). BASED in LOCAL-STORAGE since 2026-10-06 (2002/basedlocal). **ruling**: EC-BOUND-PTR (13.18.5.4 rule 4, an address that is not valid storage) is not raised -- the machine faults on such an address, and no check short of that tells a valid one from an invalid one |
| PROGRAM-POINTER | 8.5.2.15, 13.18.60 (USAGE PROGRAM-POINTER [TO program-prototype-name], GR 26's shape): a program's entry address, NULL, or restricted to a prototype's signature | **test**: 2002/pgpointer (no oracle: GnuCOBOL 4 has no ADDRESS OF PROGRAM); a program-pointer at level 1 as a data-pointer is (rule 14) |
| ADDRESS OF PROGRAM | 8.4.3.13: by a literal, an alphanumeric or national item holding the name, or a prototype-name (rule 3: a REPOSITORY program-specifier; GR 3: the value restricted to it); GR 1-2: the registry's entry for the externalized name, a contained program by its scope; GR 4: not found, NULL and EC-PROGRAM-NOT-FOUND | **test**: 2002/pgpointer (all three forms; "nowhere" is NULL; the checked case run by hand: fatal EC-PROGRAM-NOT-FOUND); **refused**: a zero-length literal (rule 2), an item of another category |
| SET format 9 | 14.9.39 (rules 21-22, GR 16): the receivers program-pointers, the value ADDRESS OF PROGRAM, a program-pointer or NULL; a restricted receiver takes NULL or a value restricted to the same prototype | **test**: 2002/pgpointer; **refused**: bad/std2002-pptr-category (a program-pointer value for a data-pointer, and the reverse), bad/std2002-pptr-restricted (rule 22). SET ... TO ENTRY, IBM's form, is refused naming ADDRESS OF PROGRAM |
| CALL program-pointer | 14.9.4 (GR 3b): the entry the pointer holds; NULL: EC-PROGRAM-PTR-NULL, the ON EXCEPTION phrase when written, else the run stops; restricted to a prototype, the arguments are checked and converted as a CALL by the prototype's name (14.8.2.3.3 rule 2) | **test**: 2002/pgpointer (`call pg using by content v`, 42.5 into 9(3); `w + 1`; ON EXCEPTION for NULL; the checked fatal case and the plain stop run by hand); **refused**: bad/std2002-call-data-pointer (a data-pointer was accepted as a program's name before) |
| relations, INITIALIZE | 8.8.4.2.4: program-pointers compare with program-pointers, EQUAL and NOT EQUAL; 14.9.20: the PROGRAM-POINTER category, NULL by default, REPLACING PROGRAM-POINTER BY a value of it | **test**: 2002/pgpointer; **refused**: a data-pointer compared with a program-pointer ("a program-pointer is compared with a program-pointer, not a data-pointer") |
| FUNCTION-POINTER | 8.5.2.7, 13.18.60 (USAGE FUNCTION-POINTER TO function-prototype-name, GR 26): a function's entry address, NULL, restricted to the prototype's signature; the TO phrase is the format's, required | **implemented** under -std=2014 (standard-queue item 26, 2026-10-07): **test**: 2014/fnpointer (no oracle: GnuCOBOL 4 has no function pointers); **refused**: bad/std2014-fnptr-no-to, -fnptr-value (13.18.63.3 rule 9, by the pointer VALUE rule), bad/std2002-function-pointer (2014's); a function-pointer at level 1 as a data-pointer is (rule 14) |
| ADDRESS OF FUNCTION | 8.4.3.12: by a prototype-name of the REPOSITORY (GR 3: the value restricted to it) or an alphanumeric or national item holding the externalized name (rules 1-2); GR 1-2: the function's entry; GR 4: not found, NULL and EC-FUNCTION-NOT-FOUND | **test**: 2014/fnpointer (by `plus`, by `"fn-times"` in an item, "nobody" is NULL; the checked case run by hand: fatal EC-FUNCTION-NOT-FOUND). By a prototype the entry is the function's link symbol; by an item the function registry (libcob `cob_register_fn`, which every function unit joins at start-up, as a program joins the program registry) answers at run time. **Refused**: bad/std2014-fnptr-address-item (rule 1), -fnptr-receiver (rule 3), bad/std2002-address-of-function (2014's) |
| SET format 8 | 14.9.39 (rule 20, GR 15): the receivers function-pointers, the value ADDRESS OF FUNCTION, a function-pointer or NULL; the two prototypes of the same signature | **test**: 2014/fnpointer; **refused**: bad/std2014-fnptr-signature (`plus` into a pointer TO `neg`: `fnsig_same`, udf.h -- as many parameters, each conforming and passed the same way, OPTIONAL alike, the returning items conforming), -fnptr-set-category (a data-pointer value). An ADDRESS OF FUNCTION identifier carries no prototype and is taken: the name is the program's business at run time. GR 15 (the effect on the function): none, an address is read |
| function-pointer-name (arguments) | 8.4.3.2 (SR 2: FUNCTION optional; SR 4-5: a function-pointer item, the parentheses required; SR 10, 13: the prototype's signature; GR 6c: NULL is EC-FUNCTION-PTR-NULL, else the run stops) | **test**: 2014/fnpointer (with and without FUNCTION; as an argument of another; from a called program's LINKAGE pointer); the pointer's value in r12 before the arguments are staged, as CALL program-pointer keeps it; the checked fatal case and the declarative run by hand. **Refused**: bad/std2014-fnptr-no-parens (rule 5: without them the item is a pointer, which DISPLAY refuses), -fnptr-arity (rule 13) |
| relations, INITIALIZE | 8.8.4.2: function-pointers compare with function-pointers and NULL; 14.9.20: the FUNCTION-POINTER category, NULL by default, REPLACING FUNCTION-POINTER BY a value of it, the implicit SET obeying rule 20 | **test**: 2014/fnpointer; **refused**: bad/std2014-fnptr-init-signature |
| ALLOCATE, FREE | 2002 14.8.3, 14.8.14 (2023 14.9.15): storage for a based record or a number of characters (rounded up), zeroed; FREE releases it and sets the pointer NULL, leaves NULL alone, and raises EC-STORAGE-NOT-ALLOC for anything else; EC-STORAGE-NOT-AVAIL when none is to be had, not for a count of 0 or less | **test**: 2002/allocfree; **refused**: bad/std2002-allocate-not-based (rule 1), -allocate-no-returning (rule 2), -allocate-returning-alnum (rule 3), -free-not-pointer (FREE rule 1). ALLOCATE data-name INITIALIZED, INITIALIZE WITH FILLER ALL TO VALUE THEN TO DEFAULT (GR 7): **test**: 2002/allocinit, the oracle agrees (a gap until ISSUES-104) |

## Rulings recorded

- `SET ADDRESS OF` a LINKAGE record at level 01 or 77 that is not
  BASED: accepted, as IBM and GnuCOBOL accept it and as older programs
  written for them do. 2023 14.9.39.3 rule 18 asks for a based item; a
  LINKAGE record is reached through the same kind of cell here.

- Under -std=85 the compiler takes, as extensions majesty uses,
  COMP-3, COMP-5, BINARY-CHAR, SIGNED-INT, SIGNED-SHORT,
  UNSIGNED-SHORT and POINTER (docs/dialect.md). The rules above apply
  to them as in 2002.
- COMP-1 is RM/COBOL's binary integer when it has a PICTURE, as every
  COMP-1 item in the Open Systems suite does; without one it is Micro
  Focus's IEEE single, and COMP-2 its double (2026-09-30; `-fcomp1=`
  forces either). IBM's hexadecimal floating point stays out.
- COMP-4 is BINARY; COMP-X and COMP-6 are Micro Focus's, and follow its
  reference (docs/usage.md): COMP-X is unsigned big-endian binary held to
  its capacity, a negative value MOVEd into it is stored in two's
  complement, and with ON SIZE ERROR a 9(n) item's digits decide the
  size error (free/compn, free/compxmf; GnuCOBOL differs, docs/oracles.md).
  COMP-6 is unsigned packed decimal, a signed one COMP-3. COMP-5 may be
  described with X's, as MF allows: n bytes, unsigned, native order
  (free/comp5x).

## Found by this sweep

Rules 8-11 and 14, and 85's rule 6, were not enforced. BINARY-SHORT
and BINARY-LONG took no SIGNED or UNSIGNED phrase. SET of a pointer to
NULL did not compile, and no pointer compared equal to NULL. ADDRESS
OF was misread as a qualified name ("'address' is not declared under
'w'"). CCVS-85, the Open Systems suite (229 programs byte-identical
under -std=85) and majesty are unaffected.
