identification division.
program-id. eclogic.
*> EC-I-O-LOGIC-ERROR (2023 9.1.13, I-O status class 4): an OPEN of a file
*> already open is 41; with no FILE STATUS and no USE procedure the run
*> would stop (USE general rule 3) -- with the condition checked its
*> declarative runs first, then the run ends (fatal).  Also EC-SORT-MERGE-
*> ACTIVE is in ecsortact.  No oracle (ecraise).
environment division.
input-output section.
file-control.
    select f assign to "tmp/eclogic.dat" organization sequential.
data division.
file section.
fd f.
01 frec pic x(4).
procedure division.
declaratives.
d1 section. use after exception condition ec-i-o-logic-error.
p1. display "  fatal: " function trim(function exception-status) " file " function exception-file.
end declaratives.
main section.
m1.
>>TURN EC-I-O-LOGIC-ERROR CHECKING ON
    open output f.
    display "open once".
    open output f.
    display "not reached".
    stop run.
