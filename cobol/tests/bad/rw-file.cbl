identification division.
program-id. rwfile.
*> A reserved word never names a file; cobol ISSUES-43.
environment division.
input-output section.
file-control.
    select rd assign to 'tmp/rw.dat'.
data division.
file section.
fd  rd.
01  rrec pic x.
procedure division.
    stop run.
