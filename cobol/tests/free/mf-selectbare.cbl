*> Micro Focus's dialect (-dialect=mf; BP-D1): file-control entries with
*> neither the INPUT-OUTPUT SECTION nor the FILE-CONTROL header, as
*> abrignoli_COBSOFT writes them -- SELECT straight after SPECIAL-NAMES.
*> The FILE-CONTROL header is optional in MF's reference (its File-Control
*> paragraph format); the section header the reference does not mark, but
*> MF practice leaves it out with the other, and so it is taken too.
identification division.
program-id. mf-selectbare.
environment division.
configuration section.
special-names.
    decimal-point is comma.
    select notes assign to "mfnotes.txt"
        organization line sequential.
data division.
file section.
fd notes.
01 note-line pic x(20).
working-storage section.
01 amount pic 9(3)v99 value 12,5.
01 shown  pic zz9,99.
procedure division.
    open output notes
    move "first note" to note-line write note-line
    move "second note" to note-line write note-line
    close notes
    open input notes
    read notes display note-line
    read notes display note-line
    close notes
    move amount to shown
    display shown
    stop run.
