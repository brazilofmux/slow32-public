*> -warn-extensions under -std=85: every class E point (docs/behavior-
*> points.md), each where a COBOL 85 program leaves the standard.
*> Free-form source is itself one (BP-E11).
identification division.
program-id. extevery.
environment division.
input-output section.
file-control.
    select lst assign to "ext.txt" organization line sequential.
    select ix assign to "ext.idx" organization indexed record key ixk
        file status ixs.
data division.
file section.
fd  lst.
01  lst-rec pic x(10).
fd  ix.
01  ix-rec.
    05 ixk pic 9(4).
    05 ixd pic x(6).
working-storage section.
01 longlit pic x(170) value "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL".   *> BP-E20
01  ixs    pic 99.
01  pk     pic s9(5) comp-3.
01  c5     pic 9(4) comp-5.
01  c1     pic s9(4) comp-1.
01  si     signed-int.
01  bc     binary-char.
01  pt     usage pointer.
01  my_item pic x value "x".
01  hx     pic x(2) value x"4142".
01  k      pic 9 value 0.
01  wide   pic 9(15)v999 value 0.
01  fine   pic 9v9999 value 0.
01  otab.
    05 oe  pic x occurs 1 to 3 depending on k.
screen section.
01  sc.
    05 line 1 col 1 value "hello".
procedure division.
    add fine to wide
    read lst
    initialize otab
    display "at" line 2 position 1
    call "nothing" using by value k
    move 0 to return-code
    display "p",k   *> BP-E22
    exit program move 0 to return-code   *> BP-E21
    stop run returning k.
    goback.
