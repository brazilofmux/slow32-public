*> Conditional compilation (2023 7.3.5-8, 7.3.11, 7.3.13, 7.3.16; cobol
*> standard-queue item 6): >>DEFINE with literals, arithmetic, OFF,
*> OVERRIDE and PARAMETER (-D, not given here: so not defined); >>IF
*> with ELSE, nested, on defined conditions, relations and AND/OR/NOT;
*> >>EVALUATE on a value with THROUGH and OTHER, and on TRUE; a COPY in
*> an omitted branch is never read (the copybook does not exist); a
*> variable defined in library text is known after the COPY; CONSTANT
*> ... FROM takes the value in effect where it stands.
*> No oracle: GnuCOBOL 4 refuses a compilation variable in a >>DEFINE
*> expression (WIDTH AS LEVEL * 4 + 1), which 7.3.11.4 rule 1 allows.
>>DEFINE LEVEL AS 3
>>DEFINE NAME AS "slow"
>>DEFINE WIDTH AS LEVEL * 4 + 1
>>DEFINE FROMENV AS PARAMETER
identification division.
program-id. condcomp.
data division.
working-storage section.
01  w-width constant from WIDTH.
>>DEFINE WIDTH AS 20 OVERRIDE
01  w-width2 constant from WIDTH.
>>IF NAME = "slow" AND LEVEL >= 3
01  greeting pic x(12) value "level 3 on".
>>ELSE
01  greeting pic x(12) value "too low".
>>END-IF
procedure division.
    display greeting
    display "width " w-width " then " w-width2
>>IF FROMENV IS DEFINED
    display "FROMENV is defined"
>>ELSE
    display "FROMENV is not defined"
>>END-IF
>>IF LEVEL < 2
    copy "no-such-copybook".
>>END-IF
    copy "condcomp-lib.cpy".
>>IF FROM-LIB = 7
    display "after the copy: FROM-LIB is 7"
>>END-IF
>>EVALUATE LEVEL
>>WHEN 1
    display "evaluate: one"
>>WHEN 2 THROUGH 4
    display "evaluate: two to four"
>>  IF NOT (LEVEL = 3)
    display "nested: not three"
>>  ELSE
    display "nested: three"
>>  END-IF
>>WHEN OTHER
    display "evaluate: other"
>>END-EVALUATE
>>EVALUATE TRUE
>>WHEN LEVEL > 5
    display "truth: above five"
>>WHEN NAME = "SLOW"
    display "truth: upper case (not taken: by encoding)"
>>WHEN OTHER
    display "truth: other"
>>END-EVALUATE
>>DEFINE LEVEL OFF
>>IF LEVEL IS NOT DEFINED
    display "LEVEL is off"
>>END-IF
    stop run.
