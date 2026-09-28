identification division.
program-id. ecio.
*> EC-I-O from the I-O status (COBOL 2002 9.1.13, 9.1.12, USE general
*> rule 3; cobol ISSUES-58).  With checking on: a READ past the end with
*> no AT END phrase raises EC-I-O-AT-END (nonfatal: its declarative, then
*> on); with an AT END phrase only the phrase runs; a duplicate alternate
*> key (02) raises EC-I-O-WARNING, turned on by its own name; a file's own
*> USE AFTER ERROR procedure comes first, and the run goes on after it
*> even for a fatal status (the implementor's choice, 9.1.13); a missing
*> file with none (35) raises EC-I-O-PERMANENT-ERROR, fatal: its
*> declarative, then the run ends.  No oracle (ecraise).
environment division.
input-output section.
file-control.
    select sq assign to "tmp/ecio-seq.dat" organization sequential.
    select ix assign to "tmp/ecio-idx.dat" organization indexed
        access dynamic record key ix-key
        alternate record key ix-alt with duplicates.
    select gone assign to "tmp/ecio-none-here.dat" organization sequential.
    select gone2 assign to "tmp/ecio-none-there.dat" organization sequential.
data division.
file section.
fd  sq.
01  sq-rec       pic x(4).
fd  ix.
01  ix-rec.
    05 ix-key    pic 9(3).
    05 ix-alt    pic x(2).
fd  gone.
01  gone-rec     pic x(4).
fd  gone2.
01  gone2-rec    pic x(4).
procedure division.
declaratives.
gone-error section.
    use after error procedure on gone.
g1.
    display "  gone's own USE AFTER ERROR procedure".
io-any section.
    use after exception condition ec-i-o.
i1.
    display "  EC-I-O declarative: " function exception-status
            " in " function exception-statement(1:5).
end declaratives.
main section.
m1.
    open output sq  write sq-rec from "abcd"  close sq
    open output ix
    move 1 to ix-key  move "aa" to ix-alt  write ix-rec
>>TURN EC-I-O CHECKING ON WITH LOCATION
    open input sq
    read sq
    display "read 1: " sq-rec
    display "read past the end, no AT END phrase:"
    read sq
    close sq
    open input sq
    read sq
    display "read past the end, with AT END:"
    read sq at end display "  the AT END phrase" end-read
    close sq
    display "duplicate alternate key, warning off:"
    move 2 to ix-key  write ix-rec
>>TURN EC-I-O-WARNING CHECKING ON WITH LOCATION
    display "duplicate alternate key, warning on:"
    move 3 to ix-key  write ix-rec
    close ix
    display "a missing file with its own USE procedure:"
    open input gone
    display "on after it"
    display "a missing file with none:"
    open input gone2
    display "not reached"
    stop run.
end program ecio.
