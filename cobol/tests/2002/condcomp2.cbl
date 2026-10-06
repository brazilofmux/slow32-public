*> 2002/condcomp without what GnuCOBOL 4 lacks, so that it is the oracle:
*> no compilation variable in a >>DEFINE expression, and no >>EVALUATE
*> (it warns "ignoring invalid directive" and keeps every branch).  One
*> documented divergence (docs/oracles.md, condcomp2.oracle-expected):
*> GnuCOBOL gives CONSTANT ... FROM the variable's last value, not the
*> one in effect where the entry stands (2023 7.3.4 rule 5).
>>DEFINE LEVEL AS 3
>>DEFINE NAME AS "slow"
>>DEFINE WIDTH AS 13
>>DEFINE FROMENV AS PARAMETER
identification division.
program-id. condcomp2.
data division.
working-storage section.
01  w-width constant from WIDTH.
>>DEFINE WIDTH AS 20 OVERRIDE
01  w-width2 constant from WIDTH.
>>IF NAME = "slow"
>>IF LEVEL >= 3
01  greeting pic x(12) value "level 3 on".
>>ELSE
01  greeting pic x(12) value "too low".
>>END-IF
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
>>IF LEVEL NOT = 3
    display "nested: not three"
>>ELSE
    display "nested: three"
>>END-IF
>>DEFINE LEVEL OFF
>>IF LEVEL IS NOT DEFINED
    display "LEVEL is off"
>>END-IF
    stop run.
