*> Micro Focus's dialect (-dialect=mf; BP-D5): the FD first in the DATA
*> DIVISION with no FILE SECTION header, as abrignoli_COBSOFT's 29
*> programs begin it (their FDs come by COPY).  MF's reference does not
*> mark the header optional; its practice leaves it out.
identification division.
program-id. mf-nofilesec.
environment division.
input-output section.
file-control.
    select notes assign to "mfnfs.txt" organization line sequential.
data division.
fd notes.
01 note-line pic x(16).
working-storage section.
01 eof pic x value "n".
procedure division.
    open output notes
    move "a file record" to note-line write note-line
    close notes
    open input notes
    read notes at end move "y" to eof end-read
    display note-line
    close notes
    stop run.
