identification division.
program-id. uev.
*> UNTIL EXIT is not the condition of a VARYING or AFTER phrase (2023
*> 14.9.28.3 rule 8).
data division.
working-storage section.
01 i pic 9.
procedure division.
    perform varying i from 1 by 1 until exit
        if i > 2 exit perform end-if
    end-perform
    stop run.
