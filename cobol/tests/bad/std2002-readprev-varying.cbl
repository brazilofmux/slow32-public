identification division.
program-id. p4.
*> READ PREVIOUS of a sequential file of variable-length records: a
*> ruling (docs/conformance/io-statements.md) -- the record before the
*> current one has no fixed place to step back to.  Fixed-length records
*> step back: 2002/startseq.
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization sequential.
data division.
file section.
fd f record varying from 3 to 10 depending on n.
01 r pic x(10).
working-storage section.
01 n pic 99.
procedure division.
    open input f
    read f previous
    close f
    goback.
