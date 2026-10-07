*> Dynamic file assignment (2023 9.1.21, 12.4.5 ASSIGN USING data-name):
*> the item's content when the OPEN runs names the file, trailing spaces
*> dropped, so one file connector reaches two physical files in turn --
*> a MOVE between OPENs takes effect at the next OPEN (GR 3b) and not
*> before; an item of spaces, status 31 (9.1.13.6); the line sequential,
*> relative and indexed organizations; and a sort file whose work area
*> the item names.  ASSIGN TO literal USING data-name: assignusing2.
*> GnuCOBOL 4 agrees.
*> docs/conformance/files.md
identification division.
program-id. assignusing.
environment division.
input-output section.
file-control.
    select f1 assign using fname organization line sequential file status fs.
    select rl assign using rname organization relative access dynamic
        relative key rk file status fs.
    select ix assign using ixname organization indexed access dynamic
        record key ik file status fs.
    select wk assign using sdname.
data division.
file section.
fd f1.
01 r1 pic x(20).
fd rl.
01 rr pic x(6).
fd ix.
01 ir.
   05 ik pic x(2).
   05 iv pic x(4).
sd wk.
01 sr pic x(4).
working-storage section.
01 fs pic xx.
01 fname pic x(30) value spaces.
01 rname pic x(30) value "tmp/au.rel".
01 ixname pic x(30) value "tmp/au.idx".
01 sdname pic x(30) value "tmp/au-sort".
01 rk pic 99.
procedure division.
*>  nothing to open yet
    open output f1
    display "blank  " fs
*>  one connector, two files
    move "tmp/au-one.txt" to fname
    open output f1 write r1 from "first file" close f1
    move "tmp/au-two.txt" to fname
    open output f1 write r1 from "second file" close f1
    move "tmp/au-one.txt" to fname
    open input f1
    move "tmp/au-two.txt" to fname
    read f1 display "one    " r1
    close f1
    open input f1 read f1 display "two    " r1 close f1
*>  relative and indexed
    open output rl
    move 2 to rk move "two   " to rr write rr
    close rl
    move "tmp/au-b.rel" to rname
    open output rl
    move 1 to rk move "one   " to rr write rr
    close rl
    open input rl read rl next display "rel b  " rk " " rr close rl
    move "tmp/au.rel" to rname
    open input rl read rl next display "rel a  " rk " " rr close rl
    open output ix
    move "k1aaaa" to ir write ir
    close ix
    move "tmp/au-b.idx" to ixname
    open output ix
    move "k2bbbb" to ir write ir
    close ix
    open input ix read ix next display "idx b  " ir close ix
    move "tmp/au.idx" to ixname
    open input ix read ix next display "idx a  " ir close ix
*>  a sort file named by its item
    sort wk on ascending key sr
        input procedure give-some
        output procedure take-some
    stop run.
give-some.
    move "c" to sr release sr
    move "a" to sr release sr
    move "b" to sr release sr.
take-some.
    perform until exit
        return wk at end exit perform end-return
        display "sorted " sr
    end-perform.
