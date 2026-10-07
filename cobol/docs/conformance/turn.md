# 7.3.25 TURN directive

Swept 2026-09-28 (ISSUES-96). COBOL 2002 and later.

## Syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | a word beginning EC- is an exception-name, not a file-name | **test**: 2002/ecturnfile |
| 2 | exception-names from 14.6.13.1's list; no check that a user name or file-name is used | **refused**: bad/std2002-turn-unknown; EC-USER-suffix names are accepted as met |
| 3 | no exception-name and file-name pair twice in one directive | **refused**: bad/std2002-turn-dup -- accepted before this sweep |
| 4 | a file-name only after an EC-I-O name | **refused**: bad/std2002-turn-file |
| 5 | no TURN inside an exception-checking PERFORM | **refused**: bad/std2002-ecp-turn (ISSUES-94 E13) |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | by default every condition's checking is off | **test**: 2002/ecraise (a RAISE before any TURN does nothing) |
| 2 | EC-ALL is every name but EC-I-O-WARNING | **test**: 2002/ecturn |
| 3 | a level-2 name is its level-3 names; with a file-name, each for that file | **test**: 2002/ecturn, ecturnfile |
| 4 | EC-I-O-WARNING only by its own name (or a WHEN), on and off | **test**: 2002/ecio |
| 5 | a TURN inside a statement applies from the next statement on, not to the statement's own phrases | **test**: 2002/ecturnstmt (a TURN in an IF's THEN branch applies to the RAISE after it and to the ELSE branch's, not to the one before) -- added by this sweep; the phrase case follows from checking being decided as each statement is compiled |
| 6 | ON enables checking for what follows in the compilation group, for one file with a file-name | **test**: 2002/ecturn, ecturnfile; 2002/turncomma -- a directive after a numeric literal written with the decimal comma (DECIMAL-POINT IS COMMA) applied one statement late until 2026-10-07: the directives are kept by token position, and the decimal-point pass joined `45296,5` into one token without moving them (copy.h `apply_decimal_point`; found by standard-queue item 27) |
| 7 | LOCATION makes the location known; without it, the implementor says | **ruling**: without LOCATION, EXCEPTION-LOCATION is spaces and EXCEPTION-STATEMENT is not recorded (2002/ecloc) |
| 8 | OFF disables likewise until an ON | **test**: 2002/ecturn |
