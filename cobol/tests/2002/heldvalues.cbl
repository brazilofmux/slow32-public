identification division.
program-id. heldvalues.
*> A binary or DISPLAY integer loaded or stored is held in a register, and
*> a later load takes it from there -- when nothing between could have
*> changed the item (src/cobc/loopreg.h, "the same reading does a second
*> thing").  Each line here reads an item, changes it some way that is
*> not a store to it by name, and reads it again: through a
*> redefinition, the group (moved to, initialized, a byte of it), a
*> table over it, a performed paragraph, READ INTO, a READ whose record
*> area it is in, the FILE STATUS a WRITE leaves, a store the runtime
*> makes, a BASED item at its address, a RELATIVE KEY; on one side of an
*> IF and not the other; and then the plain stores, after which the
*> register is right.  The second reading must see the change.
environment division.
input-output section.
file-control.
    select f1 assign to "tmp/heldvalues.dat" organization sequential.
    select f2 assign to "tmp/heldvalues2.dat" organization sequential
        file status is st.
    select relf assign to "tmp/heldvalues.rel" organization relative
        access sequential relative key RK.
data division.
file section.
fd  f1.
01  f1-rec.
    05  f1-n     pic 9(4) comp.
    05  f1-m     pic 9(4) comp.
fd  f2.
01  f2-rec       pic x(4).
fd  relf.
01  relf-rec     pic x(4).
working-storage section.
01  G.
    05  I        pic 9(4) comp.
    05  N        pic 9(4) comp.
01  GX redefines G.
    05  I-X      pic xx.
    05  filler   pic xx.
01  GT redefines G.
    05  T        pic 9(4) comp occurs 2.
01  GZ.
    05  filler   pic 9(4) comp value 40.
    05  filler   pic 9(4) comp value 60.
01  DG.
    05  D        pic 99.
    05  E        pic 9(4).
01  DX redefines DG pic x(6).
01  ST-AREA.
    05  st       pic xx.
01  ST-N redefines ST-AREA pic 9(4) comp.
01  A            pic 9(5).
01  B            pic 9(5).
01  K            pic 9(4) comp.
01  P            pic 9(4) comp.
01  RK           pic 9(4) comp.
01  TXT          pic x(12).
01  LK           pic 9(4) comp based.
01  SW           pic 9 value 1.
procedure division.
main-para.
    open output f1
    move 21 to f1-n  move 22 to f1-m  write f1-rec
    move 23 to f1-n  move 24 to f1-m  write f1-rec
    close f1

    move 1 to I  move I to A  move x"0003" to I-X  move I to B
    display "a redefinition:      " A " " B
    move 1 to I  move I to A  move GZ to G  move I to B
    display "the group moved to:  " A " " B
    move 1 to I  move I to A  initialize G  move I to B
    display "initialized:         " A " " B
    move 1 to I  move I to A  move x"04" to G(2:1)  move I to B
    display "a byte of the group: " A " " B
    move 1 to I  move I to A  move x"01" to G(1:1)  move I to B
    display "its first byte:      " A " " B
    move 1 to I  move 1 to K  move I to A  move 7 to T(K)  move I to B
    display "a table over it:     " A " " B
    move 1 to I  move I to A  perform bump  move I to B
    display "a paragraph:         " A " " B

    open input f1
    move 1 to I  move I to A  read f1 into G  move I to B
    display "READ INTO:           " A " " B
    move 5 to f1-n  move f1-n to A  read f1  move f1-n to B
    display "the record area:     " A " " B
    close f1

    open output f2
    move "zz" to st  move ST-N to A  move "abcd" to f2-rec  write f2-rec  move ST-N to B
    display "the status, a number: " A " " B
    close f2

    move 1 to I  move I to A  compute I rounded = 7 / 2  move I to B
    display "ROUNDED:             " A " " B
    move 1 to P  move P to A  move spaces to TXT
    string "abc" delimited by size into TXT with pointer P
    move P to B
    display "STRING's pointer:    " A " " B
    set address of LK to address of G
    move 1 to I  move I to A  move 9 to LK  move I to B
    display "a BASED item on it:  " A " " B

    open output relf
    perform 3 times move "rel." to relf-rec  write relf-rec end-perform
    close relf
    open input relf
    move 0 to RK  move RK to A  read relf next  read relf next  move RK to B
    display "the relative key:    " A " " B
    close relf

    *> DISPLAY integers: held by their digits
    move 12 to D  move D to A  move "34" to DX(1:2)  move D to B
    display "digits, as characters: " A " " B
    move 12 to D  move 5678 to E  move E to A  move "009999" to DX  move E to B  move D to K
    display "the group of them:   " A " " B " " K
    move 12 to D  move D to A  move zeros to DG  move D to B
    display "ZEROS to the group:  " A " " B

    *> one side of an IF changes it, the other does not
    move 1 to I
    if SW = 1 move x"0005" to I-X else move I to A end-if
    move I to B
    display "changed on one side: " B
    move 1 to I
    if SW = 2 move x"0005" to I-X else move I to A end-if
    move I to B
    display "... on the other:    " B
    move 1 to I
    if SW = 1 move 6 to I else move 7 to I end-if
    move I to B
    display "stored on both:      " B
    move 1 to I  move 3 to N
    if SW = 1 add I to N else subtract I from N end-if
    move I to A  move N to B
    display "read on both:        " A " " B

    *> the plain stores: the register is the item
    move 9990 to I  add 15 to I  move I to A  compute I = I * 2  move I to B
    display "past the picture:    " A " " B
    move 5 to I  move I to A  subtract 2 from I  move I to B  multiply 3 by I  move I to K
    display "stored and read:     " A " " B " " K
    stop run.
bump.
    add 2 to I.
