*> The ENVIRONMENT DIVISION paragraphs swept for docs/conformance/
*> environment.md (queue item 19d): SOURCE-COMPUTER and OBJECT-COMPUTER
*> with and without a computer-name (12.3.5, 12.3.6), PROGRAM COLLATING
*> SEQUENCE deciding comparisons and condition-names (12.3.6.4 rule 11),
*> inherited by a contained program (rule 1); I-O-CONTROL's SAME RECORD
*> AREA (one record area for two files), SAME AREA and SAME SORT AREA
*> taken as the hints they are (12.4.6.4); and the sweeps of 19c and 19e:
*> END PROGRAM markers for nested and following programs (10.7), a
*> program prototype ahead of its definition (10.6), ROUNDED MODE's eight
*> modes (14.7.4), ALLOCATE and FREE (14.9.3, 14.9.15), UNLOCK's I-O status
*> (14.9.47), TYPE with the subject's own VALUE (13.18.57.4 rule 3).
*> No oracle: GnuCOBOL 4 refuses a SAME SORT AREA naming no sort file
*> differently and has no UNLOCK status.
*> docs/conformance/environment.md
identification division.
program-id. sub1 is prototype.
data division.
linkage section.
01 arg pic x(4).
procedure division using arg.
end program sub1.
identification division.
program-id. envsweep.
environment division.
configuration section.
source-computer. slow-32.
object-computer. slow-32 program collating sequence is rev.
special-names.
    alphabet rev is "Z" thru "A".
repository.
    program sub1.
input-output section.
file-control.
    select a1 assign to "tmp/env-a1" organization line sequential file status fs.
    select a2 assign to "tmp/env-a2" organization line sequential file status fs.
    select a3 assign to "tmp/env-a3" organization line sequential file status fs.
    select wk assign to "tmp/env-wk".
i-o-control.
    same record area for a1 a2.
    same area for a1 a2.
    same sort area for wk a1 a2 a3.
data division.
file section.
fd a1.
01 r1 pic x(4).
fd a2.
01 r2 pic x(4).
fd a3.
01 r3 pic x(4).
sd wk.
01 wr pic x(4).
working-storage section.
01 fs pic xx.
01 w pic x value "A".
   88 w-late value "Z" thru "A".
01 rnd pic 9v9.
01 neg pic s9v9.
01 pt typedef.
   05 px pic 9(3) value 5.
   05 py pic x(2) value "ab".
01 q type pt value "123ab".
01 r type pt.
01 b based.
   05 bx pic x(4).
01 p usage pointer.
01 n pic 9(3) value 10.
procedure division.
    if w > "Z" display "A after Z" end-if
    if w-late display "88 by the sequence" end-if
    call "inner"
    open output a1
    move "abcd" to r1 write r1
    display "a2 shares " r2
    close a1
    unlock a1
    display "unlock closed " fs
    open input a1
    unlock a1 records
    display "unlock open " fs
    close a1
    compute rnd rounded mode nearest-even = 1.25 display "nearest-even " rnd
    compute rnd rounded mode nearest-away-from-zero = 1.25 display "nearest-away " rnd
    compute rnd rounded mode nearest-toward-zero = 1.25 display "nearest-toward " rnd
    compute rnd rounded mode away-from-zero = 1.21 display "away " rnd
    compute rnd rounded mode toward-greater = 1.21 display "greater " rnd
    compute neg rounded mode toward-lesser = -1.21 display "lesser " neg
    compute rnd rounded mode truncation = 1.29 display "truncation " rnd
    compute rnd rounded mode prohibited = 1.25 on size error display "prohibited " rnd end-compute
    display "type value " q " " r
    allocate b initialized
    display "allocated [" bx "]"
    set p to address of b
    free p
    if p = null display "freed" end-if
    allocate n * 2.5 characters returning p
    if p not = null display "25 characters" end-if
    free p
    allocate 0 characters returning p
    if p = null display "zero characters: null" end-if
    move "abcd" to r1
    call sub1 using r1
    stop run.
identification division.
program-id. inner.
data division.
working-storage section.
01 x pic x value "B".
procedure division.
    if x < "A" display "inner inherits the sequence" end-if
    goback.
end program inner.
end program envsweep.
identification division.
program-id. sub1.
data division.
linkage section.
01 arg pic x(4).
procedure division using arg.
    display "sub1 " arg
    goback.
end program sub1.
