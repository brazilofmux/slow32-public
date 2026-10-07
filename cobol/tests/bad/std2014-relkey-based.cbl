identification division.
program-id. p-std2014-relkey-based.
*> A RELATIVE KEY in a BASED record: its address changes with every SET
*> ADDRESS, so the file cannot hold it (EXTERNAL, LINKAGE and
*> LOCAL-STORAGE ones are bound at entry since 2026-10-07).
environment division.
input-output section.
file-control.
    select relf assign to "x.dat" organization relative access random relative key rk.
data division.
file section.
fd relf.
01 relf-rec pic x(8).
working-storage section.
01 b based.
   05 rk pic 9(4).
procedure division.
    stop run.
