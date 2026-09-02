*> decadd -- ADD x TO r and SUBTRACT x FROM r between DISPLAY and PACKED-DECIMAL
*> items of one scale, in line: two limbs of base 10^9 and a sign, no
*> runtime call (GitHub #29, ISSUES-26: the batch's largest remaining
*> runtime cost was this statement, at ~900 instructions).  Signs crossing
*> zero both ways, a zero result's sign, the carry between the limbs, the
*> truncation past the picture, an even digit count (a packed pad nibble),
*> a DISPLAY receiver's overpunch, an unsigned receiver taking the
*> magnitude, eighteen digits, a subscripted receiver, two receivers, the
*> item added to itself, ROUNDED with nothing to round.
identification division.
program-id. decadd.
data division.
working-storage section.
01  amt  pic 9(9)v99 value 1234567.89.
01  pk   pic s9(9)v99 packed-decimal value -1234567.89.
01  tot  pic s9(9)v99 packed-decimal value 0.
01  utot pic 9(9)v99 packed-decimal value 0.
01  d10  pic s9(8)v99 packed-decimal value 0.
01  dsp  pic s9(9)v99 value 0.
01  udsp pic 9(9)v99 value 0.
01  big  pic s9(16)v99 packed-decimal value 0.
01  bigo pic 9(16)v99 value 9999999999999999.99.
01  bigd pic s9(16)v99 value -1.01.
01  tbl.
    05  e occurs 3 pic s9(9)v99 packed-decimal.
01  ix   pic 9(4) comp value 2.
01  ed   pic -(9)9.99.
01  edb  pic -(16)9.99.
procedure division.
main-procedure.
    move 0 to e (1) e (2) e (3)
    add amt to tot
    move tot to ed display 'A ' ed
    add pk to tot
    move tot to ed display 'B ' ed
    add pk to tot
    move tot to ed display 'C ' ed
    add amt to tot
    move tot to ed display 'D ' ed
    subtract amt from tot
    move tot to ed display 'E ' ed
    subtract pk from tot
    move tot to ed display 'F ' ed
    add amt to utot
    subtract pk from utot
    move utot to ed display 'G ' ed
    move 3 to utot
    subtract amt from utot
    move utot to ed display 'H ' ed
    move 999999999.99 to tot
    move 0.01 to udsp
    add udsp to tot
    move tot to ed display 'I ' ed
    move 9999999.99 to tot
    add udsp to tot
    move tot to ed display 'J ' ed
    move -9999999.99 to tot
    subtract udsp from tot
    move tot to ed display 'J2 ' ed
    add amt to d10
    move d10 to ed display 'K ' ed
    move 2000000 to udsp
    subtract udsp from d10
    move d10 to ed display 'K2 ' ed
    add pk to dsp
    move dsp to ed display 'L ' ed
    add amt to dsp
    move dsp to ed display 'L2 ' ed
    subtract amt from dsp
    move dsp to ed display 'L3 ' ed
    add dsp to dsp
    move dsp to ed display 'L4 ' ed
    display 'L5 ' dsp
    move 0 to udsp
    add amt to udsp
    add amt to udsp
    move udsp to ed display 'M ' ed
    move 1 to udsp
    subtract amt from udsp
    move udsp to ed display 'M2 ' ed
    add bigo to big
    move big to edb display 'N ' edb
    add bigo to big
    move big to edb display 'N2 ' edb
    subtract bigo from big
    subtract bigo from big
    add bigd to big
    move big to edb display 'N3 ' edb
    subtract big from bigd
    move bigd to edb display 'N4 ' edb
    add amt to e (ix)
    add e (ix) to e (1)
    move e (1) to ed display 'O ' ed
    move e (3) to ed display 'O2 ' ed
    move 0 to tot
    move 0 to utot
    add amt to tot utot
    subtract pk from tot utot
    move tot to ed display 'P ' ed
    move utot to ed display 'P2 ' ed
    move -5 to dsp
    add dsp to tot
    move tot to ed display 'Q ' ed
    add amt to tot rounded
    move tot to ed display 'R ' ed
    stop run.
end program decadd.
