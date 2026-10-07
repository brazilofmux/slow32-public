identification division.
program-id. p-std2023-close-lock.
*> COBOL 2023 removed CLOSE WITH LOCK and status 38 (Annex E.2 item 1; BP-R3).
environment division.
input-output section.
file-control.
    select f assign to "p.dat" organization sequential.
data division.
file section.
fd f.
01 frec pic x(10).
procedure division.
    open output f close f with lock
    stop run.
