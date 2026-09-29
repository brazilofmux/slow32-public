identification division.
program-id. uet.
*> PERFORM UNTIL EXIT takes no WITH TEST phrase (2023 14.9.28.3 rule 8;
*> cobol ISSUES-94 E15).
data division.
working-storage section.
01  i pic 9 value 0.
procedure division.
m1.
    perform with test after until exit
        add 1 to i
        if i > 2 exit perform end-if
    end-perform
    stop run.
