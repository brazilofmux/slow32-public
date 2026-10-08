# 14.9.49 USE statement

Swept 2026-09-28 (ISSUES-96). X3.23-1985: the USE statement (Nucleus and I-O modules),
XIII (USE BEFORE REPORTING). 2023 formats: 1 file exception, 2
reporting, 3 exception-name, 4 exception object. CCVS-85 exercises
formats 1 and 2: SQ103A, SQ105A, SQ121A-SQ135A (USE AFTER ERROR), IC233A
and IC234A (USE GLOBAL across contained programs), the RW module.

## Syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | USE immediately follows its section header in the declaratives, a sentence by itself | **refused**: bad/use-not-first, bad/use-not-alone -- neither checked before this sweep ("USE must be the first sentence" was said only with no section at all) |
| 2 | no sort or merge file in USE | **refused**: bad/use-sort-file -- accepted before this sweep |
| 3 | a declarative procedure refers to no nondeclarative procedure (except in RESUME) | **refused**: bad/use-refers-main (GO TO, PERFORM, ALTER) -- accepted before this sweep; RESUME AT procedure-name is the exception (resume.md, 2026-10-07) |
| 4 | a declarative procedure is named from another section, or from outside the declaratives, only by PERFORM | **refused**: bad/use-goto-into -- accepted before this sweep |
| 5 | the files need not share organization or access | **test**: CCVS SQ module |
| 6 | ERROR and EXCEPTION are the same in format 1 | **test**: accepted (the parser takes either) |
| 7 | INPUT, OUTPUT, I-O, EXTEND each once | **refused**: "two USE procedures for the same open mode", now citing the rule |
| 8 | a file-name in one format 1 USE only | **refused**: "two USE procedures for file", now citing the rule |
| 9 | identifier-1 a report group, in one USE BEFORE REPORTING only | **refused**: "'x' is not a report group", "two USE BEFORE REPORTING procedures" |
| 10 | no GENERATE, INITIATE or TERMINATE in a USE BEFORE REPORTING procedure | **refused**: bad/rw-use-generate -- accepted before this sweep |
| 11 | a USE BEFORE REPORTING procedure alters no control item | **ruling**: not checked -- it would take a data-flow analysis of the declarative and everything it performs |
| 12 | EC is EXCEPTION CONDITION | **test**: accepted (2002/ecio, ecraise) |
| 13, 14 | FILE file-name-2 with an EC-I-O name; a pair once | **refused**: bad/std2002-use-file-io (rule 13), "is in two USE statements" (rule 14); **test**: 2002/usefile (2026-10-06) |
| 15-17 | format 4, exception objects | **n/a**: object orientation |
| format | format 3 takes no GLOBAL | **refused**: "USE GLOBAL is not allowed with EXCEPTION CONDITION", now citing the format |
| -- | two format 3 USE statements naming the same exception-name | **test**: 2002/usedupec -- refused before this sweep, which no rule supports; general rule 3 takes the first in the source |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | USE sets when declaratives run | **test**: CCVS SQ, 2002/ecraise |
| 2 | an exception that would re-enter an active USE procedure is EC-FLOW-USE | **test**: 2002/ecflowuse (ISSUES-94 E14) |
| 3a-b | format 1 USE statements first: the file's, then the open mode's | **test**: CCVS SQ121A-SQ135A; emit_use_dispatch's order |
| 3c-d | format 3 with FILE: the file's level-3 name, then its level-2 name, before any USE naming no file | **test**: 2002/usefile |
| 3e-g | format 3: the name, its group, EC-ALL | **test**: 2002/ecturn, usedupec |
| 4 | contained programs: own declaratives first, then GLOBAL ones outward | **test**: free/nestuse, CCVS IC233A/IC234A |
| 5 | a file-name USE over an open-mode USE | **test**: CCVS SQ module |
| 6 | after the standard error routine, unless AT END or INVALID KEY takes it; the WHEN of an exception-checking PERFORM first | **test**: CCVS SQ; 2002/ecpreview (E8) |
| 7 | after the procedure, continue; a fatal EC-I-O condition is the implementor's | **ruling**: without a FILE STATUS the run stops with the status (cob_io_unhandled); docs/indexed.md |
| 8, 9 | USE BEFORE REPORTING runs just before its group, after control breaks and sums | **test**: 2002/rptuse, CCVS RW module |
| 10 | GENERATE, INITIATE or TERMINATE reached at run time from a USE BEFORE REPORTING procedure is EC-FLOW-REPORT | **gap**: the static case is refused (syntax rule 10); a run-time path there (a PERFORM of another declarative) is not checked |
| 11 | the first condition in evaluation order selects the USE | **test**: the compiler raises one condition per statement |
| 12, 13 | after the procedure: EC-I-O as rule 7; another nonfatal condition continues after the statement, a fatal one ends the run | **test**: 2002/ecraise, ecio, ecpfatal |
| 14, 15 | format 4 | **n/a**: object orientation |

## Found by this sweep

Syntax rules 1 (twice over), 2, 3, 4 and 10 were not enforced; two
format 3 USE statements for one exception-name were refused, which no
rule supports. None of the Open Systems programs, CCVS-85 or majesty is
affected.
