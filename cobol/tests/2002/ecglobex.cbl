identification division.
program-id. ecglobex.
*> EC-FLOW-GLOBAL-EXIT (2023 Table 13; 14.9.14.3 rule 2 refuses an EXIT
*> PROGRAM written in a declarative whose USE is GLOBAL): one reached while
*> such a declarative is under way, through a PERFORM of a paragraph in
*> another declarative section, is the condition at run time, fatal.
*> No oracle (ecraise).  The condition's own declarative comes first in the source:
*> a condition raised inside a declarative section finds only the USE
*> procedures declared before it (the dispatch is bound where it is written).
environment division.
input-output section.
file-control.
    select f assign to "tmp/ecglobex-none.dat" organization sequential file status fs.
    select g assign to "tmp/ecglobex-g.dat" organization sequential file status gs.
data division.
file section.
fd f.
01 frec pic x(4).
fd g.
01 grec pic x(4).
working-storage section.
01 fs pic xx.
01 gs pic xx.
procedure division.
>>TURN EC-FLOW-GLOBAL-EXIT CHECKING ON
declaratives.
d0 section. use after exception condition ec-flow-global-exit.
p0. display "  fatal: " function trim(function exception-status).
d1 section. use global after standard error procedure on f.
p1. display "  global declarative: status " fs.
    perform leave-now.
    display "  not reached".
d2 section. use after standard error procedure on g.
p2. display "  g's declarative (not run)".
leave-now.
    display "  performed from the global declarative: exit program".
    exit program.
end declaratives.
main section.
m1.
    open input f.
    display "not reached: " fs.
    stop run.
