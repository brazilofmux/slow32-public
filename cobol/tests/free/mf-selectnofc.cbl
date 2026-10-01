*> Micro Focus's dialect (-dialect=mf; BP-D1): the INPUT-OUTPUT SECTION
*> header with no FILE-CONTROL paragraph header under it -- optional in
*> MF's reference (its File-Control paragraph format).
identification division.
program-id. mf-selectnofc.
environment division.
input-output section.
    select notes assign to "mfnotes2.txt"
        organization line sequential.
data division.
file section.
fd notes.
01 note-line pic x(20).
procedure division.
    open output notes
    move "only note" to note-line write note-line
    close notes
    open input notes
    read notes display note-line
    close notes
    stop run.
