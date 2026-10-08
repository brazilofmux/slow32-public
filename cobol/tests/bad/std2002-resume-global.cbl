identification division.
program-id. p-std2002-resume-global.
*> RESUME not in a GLOBAL declarative (14.9.33.3 rule 2).
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization line sequential.
data division.
file section.
fd f.
01 f-rec pic x(10).
procedure division.
declaratives.
f-sec section.
    use global after error procedure on f.
f-para.
    resume at next statement.
end declaratives.
main section.
    open input f.
    stop run.
