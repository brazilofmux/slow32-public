identification division.
program-id. p-std2014-delete-file.
*> DELETE FILE is COBOL 2023 (14.9.10 format 2).
environment division.
input-output section.
file-control.
    select f assign to "p.dat" organization sequential.
data division.
file section.
fd f.
01 frec pic x(10).
procedure division.
    delete file f
    stop run.
