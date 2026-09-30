# EVALUATE and IF: 14.9.13, 14.9.19

Swept 2026-09-29 (ISSUES-114). X3.23-1985: 6.13 EVALUATE, 6.16 IF.
2023: 14.9.13 (with Table 15, the combinations of subject and object),
14.9.19. The general rules are exercised at length by CCVS-85's NC and
IF programs, which match GnuCOBOL; this sweep went after the syntax
rules.

## EVALUATE

| rule | paraphrase | disposition |
|---|---|---|
| 85: 5; 2023: 2 | each WHEN has as many objects as there are subjects | **refused** with the rule -- too few gave "expected 'also'", too many "'also' is not a COBOL verb" |
| 85: 4; 2023: 4 | the two ends of THRU of one class | **refused**: bad/evaluate-rules -- 1 THRU "z" was accepted; ZERO goes with either |
| 85: 6a; 2023 Table 15 | objects valid to compare with their subject; a literal subject not with a literal object | **refused** -- EVALUATE 1 WHEN 2 was accepted. A subject written as a literal counts, not one the compiler folds (CCVS-85 IF115A evaluates FUNCTION LENGTH("...")) |
| 85: 6b; 2023 Table 15 | a condition, TRUE or FALSE as an object only for a subject that is TRUE, FALSE or a condition | **refused** with the rule -- "WHEN a = 1" under an identifier subject was a parse error, "WHEN TRUE" said "'true' is not declared" |
| 85: 6c | ANY for any subject | **test**: CCVS-85 |
| format | each WHEN phrase (or run of them) followed by a statement; WHEN OTHER last | **refused**: bad/evaluate-rules -- a WHEN with no statement was accepted; a WHEN after WHEN OTHER gave "'when' without a matching statement" |
| 2023 5, 7d, 8 | partial expressions (WHEN > 3) | **not implemented** (COBOL 2014), said so; under -std=85 "is COBOL 2014" |
| 2023 3 | an alphabet on THRU | **n/a**: 2014's |

## IF

| rule | paraphrase | disposition |
|---|---|---|
| 85: 1; 2023: 1 | a statement, or NEXT SENTENCE, after the condition and after ELSE | **refused**: bad/if-rules -- IF c END-IF and an empty ELSE were accepted |
| 85: 2 | ELSE NEXT SENTENCE may be omitted before the period | **test**: accepted |
| 85: 3; 2023 format 2 | NEXT SENTENCE not with END-IF | **refused**: bad/if-rules -- accepted before |
| 2023 2 | ELSE and END-IF match the nearest open IF | **test**: CCVS-85 |

CCVS-85, the Open Systems suite and majesty trip none of the refusals;
the first cut of the literal rule refused IF115A and was corrected
before commit.
