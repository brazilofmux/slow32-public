*> Micro Focus's dialect (-dialect=mf; BP-D6): ASSIGN TO a data-name
*> declared nowhere.  MF declares it implicitly, alphanumeric and long
*> enough for a file name (its SELECT rule 4), and the program fills it
*> before the OPEN -- abrignoli_COBSOFT STRINGs a path into wid-pd00100
*> and its like.  The item's length is the implementor's -- 1024 here,
*> 4095 in GnuCOBOL -- so the test does not show it.
identification division.
program-id. mf-assignimp.
environment division.
input-output section.
file-control.
    select notes assign to disk wid-notes
        organization line sequential.
data division.
file section.
fd notes.
01 note-line pic x(12).
working-storage section.
01 base pic x(8) value "mfimp".
procedure division.
    string base delimited by space ".txt" delimited by size into wid-notes
    open output notes
    move "implicit" to note-line write note-line
    close notes
    move spaces to note-line
    open input notes
    read notes end-read
    display "[" note-line "] from " wid-notes(1:9)
    if function length(wid-notes) >= 260 display "long enough for a path" end-if
    close notes
    stop run.
