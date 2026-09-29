# 14.9.29 RAISE statement

Swept 2026-09-28 (ISSUES-96). COBOL 2002 and later; under -std=85
refused (bad/raise-85).

## Syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | exception-name-1 is a level-3 exception-name | **refused**: bad/std2002-raise-level2 (a level-2 name), bad/std2002-ec-unknown (no such name) |
| 2, 3 | RAISE identifier-1: an object reference | **n/a**: object orientation, deferred (refused by name) |
| 4 | inside an exception-checking PERFORM, RAISE only in imperative-statement-1 | **refused**: bad/std2002-ecp-raise |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | the condition is raised, execution continues by 14.6.13: a fatal one ends the run after its declarative, a nonfatal one with no handler is a CONTINUE | **test**: 2002/ecraise, ecpfatal, ecflowuse. Raised only when its checking is enabled -- 14.6.13.1 ("it is raised only if checking for that exception condition is enabled"), which this compiler decides at compile time: with checking off, RAISE compiles to nothing |
| 2 | RAISE identifier-1 sets EXCEPTION-OBJECT | **n/a**: object orientation |

Nothing new found; the rules were enforced by ISSUES-53 and the Stage B
review (ISSUES-94).
