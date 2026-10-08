identification division.
program-id. ecglobal.
*> EC-FLOW-GLOBAL-GOBACK (2023 14.9.18.4 rule 6): a GOBACK written in a
*> declarative whose USE is GLOBAL is refused at compile time (14.9.18.3
*> rule 1); one reached while such a declarative is under way -- here a
*> paragraph of another declarative section, performed from the GLOBAL one
*> (14.9.49.3 rule 3 keeps a declarative's PERFORMs within the
*> declaratives) -- is the condition at run time, fatal.  The GLOBAL
*> declarative runs for the OPEN of a file that is not there.  No oracle
*> (ecraise).  The condition's own declarative comes first in the source: a
*> condition raised inside a declarative section finds only the USE
*> procedures declared before it (the dispatch is bound where it is written).
environment division.
input-output section.
file-control.
    select f assign to "tmp/ecglobal-none.dat" organization sequential file status fs.
    select g assign to "tmp/ecglobal-g.dat" organization sequential file status gs.
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
>>TURN EC-FLOW-GLOBAL-GOBACK CHECKING ON
declaratives.
d0 section. use after exception condition ec-flow-global-goback.
p0. display "  fatal: " function trim(function exception-status).
d1 section. use global after standard error procedure on f.
p1. display "  global declarative: status " fs.
    perform leave-now.
    display "  not reached".
d2 section. use after standard error procedure on g.
p2. display "  g's declarative (not run)".
leave-now.
    display "  performed from the global declarative: goback".
    goback.
end declaratives.
main section.
m1.
    open input f.
    display "not reached: " fs.
    stop run.
